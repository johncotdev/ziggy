//! Ziggy DBL Bytecode Compiler
//!
//! Transforms AST into executable bytecode modules.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const opcodes = @import("opcodes.zig");
const module = @import("module.zig");

const Opcode = opcodes.Opcode;
const Allocator = std.mem.Allocator;

/// Compiler errors
pub const CompileError = error{
    OutOfMemory,
    TooManyConstants,
    TooManyLocals,
    TooManyGlobals,
    TooManyRoutines,
    UndefinedVariable,
    UndefinedRoutine,
    UndefinedLabel,
    DuplicateLabel,
    InvalidExpression,
    InvalidStatement,
    JumpTooFar,
    StackOverflow,
    TypeMismatch,
};

/// Variable scope information
const VarInfo = struct {
    name: []const u8,
    slot: u16,
    is_global: bool,
    data_type: module.DataTypeCode,
};

/// Label for forward references
const Label = struct {
    name: []const u8,
    offset: ?u32, // null if not yet defined
    references: std.ArrayList(u32), // locations that reference this label
};

/// Bytecode compiler
pub const Compiler = struct {
    allocator: Allocator,

    // Output
    code: std.ArrayList(u8),
    constants: std.ArrayList(module.Constant),
    types: std.ArrayList(module.TypeDef),
    routines: std.ArrayList(module.RoutineDef),

    // Symbol tables
    globals: std.StringHashMap(VarInfo),
    global_count: u16,
    locals: std.StringHashMap(VarInfo),
    local_count: u16,
    labels: std.StringHashMap(Label),

    // Constant deduplication
    string_constants: std.StringHashMap(u16),
    int_constants: std.AutoHashMap(i64, u16),

    // Stack tracking
    max_stack: u16,
    current_stack: u16,

    // Debug info
    emit_debug: bool,
    current_line: u16,
    line_table: std.ArrayList(module.LineEntry),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .code = .{},
            .constants = .{},
            .types = .{},
            .routines = .{},
            .globals = std.StringHashMap(VarInfo).init(allocator),
            .global_count = 0,
            .locals = std.StringHashMap(VarInfo).init(allocator),
            .local_count = 0,
            .labels = std.StringHashMap(Label).init(allocator),
            .string_constants = std.StringHashMap(u16).init(allocator),
            .int_constants = std.AutoHashMap(i64, u16).init(allocator),
            .max_stack = 0,
            .current_stack = 0,
            .emit_debug = true,
            .current_line = 0,
            .line_table = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.types.deinit(self.allocator);
        self.routines.deinit(self.allocator);
        self.globals.deinit();
        self.locals.deinit();
        var label_iter = self.labels.valueIterator();
        while (label_iter.next()) |label| {
            label.references.deinit(self.allocator);
        }
        self.labels.deinit();
        self.string_constants.deinit();
        self.int_constants.deinit();
        self.line_table.deinit(self.allocator);
    }

    fn pushStack(self: *Self, count: u16) void {
        self.current_stack += count;
        if (self.current_stack > self.max_stack) {
            self.max_stack = self.current_stack;
        }
    }

    fn popStack(self: *Self, count: u16) void {
        if (self.current_stack >= count) {
            self.current_stack -= count;
        }
    }

    /// Compile a complete program
    pub fn compile(self: *Self, program: *const ast.Program) CompileError!module.Module {
        // First pass: collect all record definitions
        for (program.statements) |stmt| {
            switch (stmt) {
                .record => |rec| try self.compileRecordDef(&rec),
                else => {},
            }
        }

        // Second pass: collect all field declarations as globals
        for (program.statements) |stmt| {
            switch (stmt) {
                .field => |field| try self.addGlobal(field.name, dataTypeFromAst(field.data_type)),
                .common => |common| {
                    for (common.fields) |field| {
                        try self.addGlobal(field.name, dataTypeFromAst(field.data_type));
                    }
                },
                else => {},
            }
        }

        // Third pass: compile all code
        var in_proc = false;
        for (program.statements) |stmt| {
            switch (stmt) {
                .proc => {
                    in_proc = true;
                },
                .record, .field, .common, .group, .literal, .class, .namespace => {},
                else => {
                    if (in_proc) {
                        try self.compileStatement(&stmt);
                    }
                },
            }
        }

        // Emit halt at end
        try self.emit(.halt);

        // Resolve forward references
        try self.resolveLabels();

        // Build the module
        return self.buildModule();
    }

    /// Compile record definition to type
    fn compileRecordDef(self: *Self, record: *const ast.RecordDef) CompileError!void {
        const name_idx = if (record.name) |n|
            try self.addStringConstant(n)
        else
            try self.addStringConstant("__anon__");

        var fields: std.ArrayList(module.FieldDef) = .{};
        defer fields.deinit(self.allocator);

        var offset: u16 = 0;
        for (record.fields) |field| {
            const field_name_idx = try self.addStringConstant(field.name);
            const data_type = dataTypeFromAst(field.data_type);
            const size = getSizeFromDataType(field.data_type);

            try fields.append(self.allocator, .{
                .name_index = field_name_idx,
                .data_type = data_type,
                .flags = 0,
                .offset = offset,
                .size = size,
                .precision = getPrecisionFromDataType(field.data_type),
                .array_dims = &[_]u16{},
            });

            // Also add as global
            try self.addGlobal(field.name, data_type);

            offset += size;
        }

        try self.types.append(self.allocator, .{
            .type_id = @intCast(self.types.items.len),
            .kind = .record,
            .flags = 0,
            .name_index = name_idx,
            .total_size = offset,
            .fields = try fields.toOwnedSlice(self.allocator),
        });
    }

    // ========================================
    // Statement Compilation
    // ========================================

    fn compileStatement(self: *Self, stmt: *const ast.Statement) CompileError!void {
        switch (stmt.*) {
            .assignment => |assign| try self.compileAssignment(&assign),
            .if_stmt => |if_s| try self.compileIf(&if_s),
            .case_stmt => |case_s| try self.compileCase(&case_s),
            .loop => |loop| try self.compileLoop(&loop),
            .call => |call| try self.compileCall(&call),
            .xcall => |xcall| try self.compileXcall(&xcall),
            .return_stmt => |ret| try self.compileReturn(&ret),
            .goto_stmt => |g| try self.compileGoto(&g),
            .label => |lbl| try self.defineLabel(lbl.name),
            .display_stmt => |display| try self.compileDisplay(&display),
            .open_stmt => |open| try self.compileOpen(&open),
            .close_stmt => |close| try self.compileClose(&close),
            .read_stmt => |read| try self.compileRead(&read),
            .write_stmt => |write| try self.compileWrite(&write),
            .store_stmt => |store| try self.compileStore(&store),
            .delete_stmt => |del| try self.compileDelete(&del),
            .clear_stmt => |clr| try self.compileClear(&clr),
            .incr_stmt => |inc| try self.compileIncr(&inc),
            .block => |blk| {
                for (blk.statements) |s| {
                    try self.compileStatement(&s);
                }
            },
            .expression => |expr| {
                try self.compileExpression(&expr);
                try self.emit(.pop); // Discard result
                self.popStack(1);
            },
            else => {},
        }
    }

    fn compileAssignment(self: *Self, assign: *const ast.Assignment) CompileError!void {
        // Compile the value expression
        try self.compileExpression(&assign.value);

        // Store to target
        try self.compileStoreTarget(&assign.target);
    }

    fn compileStoreTarget(self: *Self, target: *const ast.Expression) CompileError!void {
        switch (target.*) {
            .identifier => |name| {
                try self.emitStoreVar(name);
                self.popStack(1);
            },
            .member => |m| {
                // Push object reference
                try self.compileExpression(&m.object);
                const field_idx = try self.addStringConstant(m.member);
                try self.emit(.store_field);
                try self.emitU16(field_idx);
                self.popStack(2);
            },
            .index => |idx| {
                try self.compileExpression(&idx.base);
                if (idx.indices.len > 0) {
                    try self.compileExpression(&idx.indices[0]);
                }
                try self.emit(.store_field);
                try self.emitU16(0);
                self.popStack(3);
            },
            else => {},
        }
    }

    fn compileIf(self: *Self, if_s: *const ast.IfStatement) CompileError!void {
        // Compile condition
        try self.compileExpression(if_s.condition);

        // Jump if false to else/end
        try self.emit(.jump_if_false);
        const else_jump = self.code.items.len;
        try self.emitU16(0); // Placeholder
        self.popStack(1);

        // Compile then branch
        try self.compileStatement(if_s.then_branch);

        if (if_s.else_branch) |else_branch| {
            // Jump over else
            try self.emit(.jump);
            const end_jump = self.code.items.len;
            try self.emitU16(0);

            // Patch else jump
            self.patchJump(else_jump);

            // Compile else branch
            try self.compileStatement(else_branch);

            // Patch end jump
            self.patchJump(end_jump);
        } else {
            // Patch else jump to here
            self.patchJump(else_jump);
        }
    }

    fn compileCase(self: *Self, case_s: *const ast.CaseStatement) CompileError!void {
        // Compile selector expression
        try self.compileExpression(&case_s.selector);

        var case_end_jumps: std.ArrayList(usize) = .{};
        defer case_end_jumps.deinit(self.allocator);

        for (case_s.cases) |case| {
            // For each case value
            for (case.values) |val| {
                // Duplicate selector for comparison
                try self.emit(.dup);
                self.pushStack(1);

                // Push case value
                try self.compileExpression(&val);

                // Compare
                try self.emit(.cmp_eq);
                self.popStack(1);

                // Jump if equal to case body
                try self.emit(.jump_if_true);
                const match_jump = self.code.items.len;
                try self.emitU16(0);
                self.popStack(1);

                // Skip to next case check...
                // For now, simplified: jump past this case's body
                _ = match_jump;
            }

            // Pop selector (matched) - simplified
            try self.emit(.pop);
            self.popStack(1);

            // Compile case body
            try self.compileStatement(case.body);

            // Jump to end
            try self.emit(.jump);
            try case_end_jumps.append(self.allocator, self.code.items.len);
            try self.emitU16(0);
        }

        // Default branch
        if (case_s.default_branch) |default| {
            try self.emit(.pop);
            self.popStack(1);
            try self.compileStatement(default);
        } else {
            try self.emit(.pop);
            self.popStack(1);
        }

        // Patch all end jumps
        for (case_end_jumps.items) |jump_loc| {
            self.patchJump(jump_loc);
        }
    }

    fn compileLoop(self: *Self, loop: *const ast.LoopStatement) CompileError!void {
        const loop_start: u32 = @intCast(self.code.items.len);

        // Init expression for for-loops
        if (loop.init_expr) |init_expr| {
            try self.compileExpression(&init_expr);
            try self.emit(.pop);
            self.popStack(1);
        }

        const condition_start: u32 = @intCast(self.code.items.len);
        var exit_jump: ?usize = null;

        // Condition check (for while loops, check at start)
        if (loop.loop_type == .while_loop) {
            if (loop.condition) |cond| {
                try self.compileExpression(&cond);
                try self.emit(.jump_if_false);
                exit_jump = self.code.items.len;
                try self.emitU16(0);
                self.popStack(1);
            }
        }

        // Body
        try self.compileStatement(loop.body);

        // Update expression for for-loops
        if (loop.update_expr) |update| {
            try self.compileExpression(&update);
            try self.emit(.pop);
            self.popStack(1);
        }

        // Condition check at end (for do-until)
        if (loop.loop_type == .do_until) {
            if (loop.condition) |cond| {
                try self.compileExpression(&cond);
                try self.emit(.jump_if_false);
                try self.emitJumpOffset(loop_start);
                self.popStack(1);
            } else {
                try self.emitJumpBack(loop_start);
            }
        } else if (loop.loop_type == .do_forever) {
            try self.emitJumpBack(loop_start);
        } else {
            // Jump back to condition
            try self.emitJumpBack(condition_start);
        }

        // Patch exit jump
        if (exit_jump) |ej| {
            self.patchJump(ej);
        }
    }

    fn compileCall(self: *Self, call: *const ast.CallStatement) CompileError!void {
        // Internal CALL - jump to label, save return address
        const name_idx = try self.addStringConstant(call.label);
        try self.emit(.call);
        try self.emitU16(name_idx);
    }

    fn compileXcall(self: *Self, xcall: *const ast.XCallStatement) CompileError!void {
        // Push arguments
        for (xcall.arguments) |arg| {
            try self.compileExpression(&arg);
        }

        const name_idx = try self.addStringConstant(xcall.routine_name);
        try self.emit(.xcall);
        try self.emitU16(name_idx);
        try self.emitU8(@intCast(xcall.arguments.len));

        self.popStack(@intCast(xcall.arguments.len));
    }

    fn compileReturn(self: *Self, ret: *const ast.ReturnStatement) CompileError!void {
        if (ret.value) |val| {
            try self.compileExpression(&val);
            try self.emit(.ret_val);
        } else {
            try self.emit(.ret);
        }
    }

    fn compileGoto(self: *Self, g: *const ast.GotoStatement) CompileError!void {
        try self.emit(.jump);
        const jump_loc = self.code.items.len;
        try self.emitU16(0);

        // Record forward reference
        try self.addLabelReference(g.label, @intCast(jump_loc));
    }

    fn compileDisplay(self: *Self, display: *const ast.DisplayStatement) CompileError!void {
        // Push channel
        try self.compileExpression(&display.channel);

        // Push each expression
        for (display.expressions) |expr| {
            try self.compileExpression(&expr);
        }

        try self.emit(.ch_display);
        try self.emitU8(@intCast(display.expressions.len));

        self.popStack(@intCast(display.expressions.len + 1));
    }

    fn compileOpen(self: *Self, open: *const ast.OpenStatement) CompileError!void {
        // Push channel
        try self.compileExpression(&open.channel);

        // Push filename
        try self.compileExpression(&open.filename);

        // Determine mode flags
        var flags = opcodes.OpenModeFlags{};
        for (open.mode) |c| {
            switch (c) {
                'I', 'i' => flags.input = true,
                'O', 'o' => flags.output = true,
                'U', 'u' => flags.update = true,
                'A', 'a' => flags.append = true,
                else => {},
            }
        }
        // Check for ISAM indicator
        if (std.mem.indexOf(u8, open.mode, ":I") != null or
            std.mem.indexOf(u8, open.mode, ":i") != null)
        {
            flags.isam = true;
        }

        try self.emit(.ch_open);
        try self.emitU8(@bitCast(flags));

        self.popStack(2);
    }

    fn compileClose(self: *Self, close: *const ast.CloseStatement) CompileError!void {
        try self.compileExpression(&close.channel);
        try self.emit(.ch_close);
        self.popStack(1);
    }

    fn compileRead(self: *Self, read: *const ast.ReadStatement) CompileError!void {
        try self.compileExpression(&read.channel);

        // For ISAM read with key
        if (read.key) |key| {
            try self.compileExpression(&key);
            try self.emit(.isam_read);
            try self.emitU8(0); // key num
            try self.emitU8(@intFromEnum(opcodes.MatchMode.exact));
            self.popStack(1);
        } else {
            try self.emit(.ch_read);
        }

        // Store to record
        try self.compileStoreTarget(&read.record);
    }

    fn compileWrite(self: *Self, write: *const ast.WriteStatement) CompileError!void {
        try self.compileExpression(&write.channel);
        try self.compileExpression(&write.record);
        try self.emit(.isam_write);
        self.popStack(2);
    }

    fn compileStore(self: *Self, store: *const ast.StoreStatement) CompileError!void {
        try self.compileExpression(&store.channel);
        try self.compileExpression(&store.record);
        try self.emit(.isam_store);
        self.popStack(2);
    }

    fn compileDelete(self: *Self, del: *const ast.DeleteStatement) CompileError!void {
        try self.compileExpression(&del.channel);
        try self.emit(.isam_delete);
        self.popStack(1);
    }

    fn compileClear(self: *Self, clr: *const ast.ClearStatement) CompileError!void {
        try self.emit(.push_null);
        self.pushStack(1);
        try self.compileStoreTarget(&clr.target);
    }

    fn compileIncr(self: *Self, inc: *const ast.IncrStatement) CompileError!void {
        // Load, increment, store
        try self.compileExpression(&inc.target);
        if (inc.amount) |amt| {
            try self.compileExpression(&amt);
            try self.emit(.add);
            self.popStack(1);
        } else {
            try self.emit(.incr);
        }
        try self.compileStoreTarget(&inc.target);
    }

    // ========================================
    // Expression Compilation
    // ========================================

    fn compileExpression(self: *Self, expr: *const ast.Expression) CompileError!void {
        switch (expr.*) {
            .integer => |val| try self.compileIntLiteral(val),
            .decimal => |val| try self.compileDecLiteral(val),
            .string => |val| try self.compileStringLiteral(val),
            .identifier => |name| try self.emitLoadVar(name),
            .null_literal => {
                try self.emit(.push_null);
                self.pushStack(1);
            },
            .binary => |bin| try self.compileBinaryOp(bin),
            .unary => |un| try self.compileUnaryOp(un),
            .call => |call| try self.compileFunctionCall(call),
            .member => |m| try self.compileMemberAccess(m),
            .index => |idx| try self.compileIndexAccess(idx),
            .range => |r| try self.compileRangeAccess(r),
            .grouping => |g| try self.compileExpression(g),
        }
    }

    fn compileIntLiteral(self: *Self, val: i64) CompileError!void {
        if (val >= -128 and val <= 127) {
            try self.emit(.push_i8);
            try self.emitU8(@bitCast(@as(i8, @intCast(val))));
        } else if (val >= -32768 and val <= 32767) {
            try self.emit(.push_i16);
            try self.emitU16(@bitCast(@as(i16, @intCast(val))));
        } else if (val >= -2147483648 and val <= 2147483647) {
            try self.emit(.push_i32);
            try self.emitU32(@bitCast(@as(i32, @intCast(val))));
        } else {
            try self.emit(.push_i64);
            try self.emitU64(@bitCast(val));
        }
        self.pushStack(1);
    }

    fn compileDecLiteral(self: *Self, val: []const u8) CompileError!void {
        // Parse the decimal string to value + precision
        var value: i64 = 0;
        var precision: u8 = 0;
        var in_fraction = false;

        for (val) |c| {
            if (c == '.') {
                in_fraction = true;
            } else if (c >= '0' and c <= '9') {
                value = value * 10 + (c - '0');
                if (in_fraction) precision += 1;
            }
        }

        const idx = try self.addDecimalConstant(value, precision);
        try self.emit(.push_const);
        try self.emitU16(idx);
        self.pushStack(1);
    }

    fn compileStringLiteral(self: *Self, val: []const u8) CompileError!void {
        const idx = try self.addStringConstant(val);
        try self.emit(.push_const);
        try self.emitU16(idx);
        self.pushStack(1);
    }

    fn compileBinaryOp(self: *Self, bin: *const ast.BinaryExpr) CompileError!void {
        try self.compileExpression(&bin.left);
        try self.compileExpression(&bin.right);

        const op: Opcode = switch (bin.operator) {
            .add => .add,
            .subtract => .sub,
            .multiply => .mul,
            .divide => .div,
            .int_divide => .div,
            .modulo => .mod,
            .power => .mul, // Simplified - would need proper power
            .equal => .cmp_eq,
            .not_equal => .cmp_ne,
            .less_than => .cmp_lt,
            .less_equal => .cmp_le,
            .greater_than => .cmp_gt,
            .greater_equal => .cmp_ge,
            .logical_and => .log_and,
            .logical_or => .log_or,
            .logical_xor => .bit_xor,
            .concat => .str_concat,
        };
        try self.emit(op);

        self.popStack(1); // Two in, one out
    }

    fn compileUnaryOp(self: *Self, un: *const ast.UnaryExpr) CompileError!void {
        try self.compileExpression(&un.operand);

        const op: Opcode = switch (un.operator) {
            .negate => .neg,
            .logical_not => .log_not,
            .bitwise_not => .bit_not,
        };
        try self.emit(op);
    }

    fn compileFunctionCall(self: *Self, call: *const ast.CallExpr) CompileError!void {
        // Check for built-in functions first
        if (getBuiltinOpcode(call.callee)) |opcode| {
            for (call.arguments) |arg| {
                try self.compileExpression(&arg);
            }
            try self.emit(opcode);
            if (call.arguments.len > 0) {
                self.popStack(@intCast(call.arguments.len - 1));
            } else {
                self.pushStack(1);
            }
            return;
        }

        // User-defined function
        for (call.arguments) |arg| {
            try self.compileExpression(&arg);
        }

        const name_idx = try self.addStringConstant(call.callee);
        try self.emit(.call_external);
        try self.emitU16(name_idx);

        // Pop args, push result
        if (call.arguments.len > 0) {
            self.popStack(@intCast(call.arguments.len));
        }
        self.pushStack(1);
    }

    fn compileMemberAccess(self: *Self, m: *const ast.MemberExpr) CompileError!void {
        try self.compileExpression(&m.object);
        const field_idx = try self.addStringConstant(m.member);
        try self.emit(.load_field);
        try self.emitU16(field_idx);
    }

    fn compileIndexAccess(self: *Self, idx: *const ast.IndexExpr) CompileError!void {
        try self.compileExpression(&idx.base);
        if (idx.indices.len > 0) {
            try self.compileExpression(&idx.indices[0]);
        } else {
            try self.emit(.push_i8);
            try self.emitU8(0);
            self.pushStack(1);
        }
        try self.emit(.load_field);
        try self.emitU16(0);
        self.popStack(1);
    }

    fn compileRangeAccess(self: *Self, r: *const ast.RangeExpr) CompileError!void {
        try self.compileExpression(&r.base);
        try self.compileExpression(&r.start);
        try self.compileExpression(&r.end_or_length);
        try self.emit(.str_slice);
        try self.emitU16(0); // Dynamic
        try self.emitU16(0);
        self.popStack(2);
    }

    // ========================================
    // Helpers
    // ========================================

    fn emit(self: *Self, op: Opcode) CompileError!void {
        try self.code.append(self.allocator, @intFromEnum(op));
    }

    fn emitU8(self: *Self, val: u8) CompileError!void {
        try self.code.append(self.allocator, val);
    }

    fn emitU16(self: *Self, val: u16) CompileError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    fn emitU32(self: *Self, val: u32) CompileError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    fn emitU64(self: *Self, val: u64) CompileError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    fn emitJumpOffset(self: *Self, target: u32) CompileError!void {
        const current: i32 = @intCast(self.code.items.len + 2);
        const offset: i16 = @intCast(@as(i32, @intCast(target)) - current);
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(offset));
    }

    fn emitJumpBack(self: *Self, target: u32) CompileError!void {
        try self.emit(.jump);
        try self.emitJumpOffset(target);
    }

    fn patchJump(self: *Self, jump_loc: usize) void {
        const current: i32 = @intCast(self.code.items.len);
        const target: i32 = @intCast(jump_loc + 2);
        const offset: i16 = @intCast(current - target);
        const bytes = std.mem.toBytes(offset);
        self.code.items[jump_loc] = bytes[0];
        self.code.items[jump_loc + 1] = bytes[1];
    }

    fn addGlobal(self: *Self, name: []const u8, data_type: module.DataTypeCode) CompileError!void {
        if (self.globals.contains(name)) return;
        if (self.global_count >= 65535) return CompileError.TooManyGlobals;
        const slot = self.global_count;
        self.global_count += 1;
        try self.globals.put(name, .{
            .name = name,
            .slot = slot,
            .is_global = true,
            .data_type = data_type,
        });
    }

    fn emitLoadVar(self: *Self, name: []const u8) CompileError!void {
        // Check locals first
        if (self.locals.get(name)) |info| {
            switch (info.slot) {
                0 => try self.emit(.load_local_0),
                1 => try self.emit(.load_local_1),
                2 => try self.emit(.load_local_2),
                3 => try self.emit(.load_local_3),
                else => {
                    if (info.slot < 256) {
                        try self.emit(.load_local);
                        try self.emitU8(@intCast(info.slot));
                    } else {
                        try self.emit(.load_local_wide);
                        try self.emitU16(info.slot);
                    }
                },
            }
            self.pushStack(1);
            return;
        }
        // Check globals
        if (self.globals.get(name)) |info| {
            try self.emit(.load_global);
            try self.emitU16(info.slot);
            self.pushStack(1);
            return;
        }
        // Auto-create global
        try self.addGlobal(name, .alpha);
        if (self.globals.get(name)) |info| {
            try self.emit(.load_global);
            try self.emitU16(info.slot);
            self.pushStack(1);
        }
    }

    fn emitStoreVar(self: *Self, name: []const u8) CompileError!void {
        // Check locals first
        if (self.locals.get(name)) |info| {
            switch (info.slot) {
                0 => try self.emit(.store_local_0),
                1 => try self.emit(.store_local_1),
                2 => try self.emit(.store_local_2),
                3 => try self.emit(.store_local_3),
                else => {
                    if (info.slot < 256) {
                        try self.emit(.store_local);
                        try self.emitU8(@intCast(info.slot));
                    } else {
                        try self.emit(.store_local_wide);
                        try self.emitU16(info.slot);
                    }
                },
            }
            return;
        }
        // Check globals
        if (self.globals.get(name)) |info| {
            try self.emit(.store_global);
            try self.emitU16(info.slot);
            return;
        }
        // Auto-create global
        try self.addGlobal(name, .alpha);
        if (self.globals.get(name)) |info| {
            try self.emit(.store_global);
            try self.emitU16(info.slot);
        }
    }

    fn addStringConstant(self: *Self, str: []const u8) CompileError!u16 {
        if (self.string_constants.get(str)) |idx| {
            return idx;
        }
        if (self.constants.items.len >= 65535) return CompileError.TooManyConstants;
        const idx: u16 = @intCast(self.constants.items.len);
        // Dupe the string so it can be freed when module is deinitialized
        const duped = self.allocator.dupe(u8, str) catch return CompileError.OutOfMemory;
        try self.constants.append(self.allocator, .{ .identifier = duped });
        try self.string_constants.put(str, idx);
        return idx;
    }

    fn addDecimalConstant(self: *Self, value: i64, precision: u8) CompileError!u16 {
        if (self.constants.items.len >= 65535) return CompileError.TooManyConstants;
        const idx: u16 = @intCast(self.constants.items.len);
        try self.constants.append(self.allocator, .{ .decimal = .{ .value = value, .precision = precision } });
        return idx;
    }

    fn defineLabel(self: *Self, name: []const u8) CompileError!void {
        const offset: u32 = @intCast(self.code.items.len);
        if (self.labels.getPtr(name)) |label| {
            if (label.offset != null) return CompileError.DuplicateLabel;
            label.offset = offset;
        } else {
            try self.labels.put(name, .{
                .name = name,
                .offset = offset,
                .references = .{},
            });
        }
    }

    fn addLabelReference(self: *Self, name: []const u8, ref_loc: u32) CompileError!void {
        if (self.labels.getPtr(name)) |label| {
            try label.references.append(self.allocator, ref_loc);
        } else {
            var refs: std.ArrayList(u32) = .{};
            try refs.append(self.allocator, ref_loc);
            try self.labels.put(name, .{
                .name = name,
                .offset = null,
                .references = refs,
            });
        }
    }

    fn resolveLabels(self: *Self) CompileError!void {
        var iter = self.labels.iterator();
        while (iter.next()) |entry| {
            const label = entry.value_ptr;
            const target = label.offset orelse return CompileError.UndefinedLabel;
            for (label.references.items) |ref_loc| {
                const current: i32 = @intCast(ref_loc + 2);
                const offset: i16 = @intCast(@as(i32, @intCast(target)) - current);
                const bytes = std.mem.toBytes(offset);
                self.code.items[ref_loc] = bytes[0];
                self.code.items[ref_loc + 1] = bytes[1];
            }
        }
    }

    fn buildModule(self: *Self) CompileError!module.Module {
        var mod = module.Module.init(self.allocator);

        mod.header.section_count = 4; // constants, types, routines, code
        mod.header.entry_point = 0; // Main is at offset 0

        mod.constants = try self.constants.toOwnedSlice(self.allocator);
        mod.types = try self.types.toOwnedSlice(self.allocator);
        mod.routines = try self.routines.toOwnedSlice(self.allocator);
        mod.code = try self.code.toOwnedSlice(self.allocator);

        if (self.emit_debug) {
            mod.line_table = try self.line_table.toOwnedSlice(self.allocator);
        }

        return mod;
    }
};

// ========================================
// Helper Functions
// ========================================

fn dataTypeFromAst(dt: ast.DataType) module.DataTypeCode {
    return switch (dt) {
        .alpha => .alpha,
        .decimal => .decimal,
        .implied_decimal => .implied_decimal,
        .integer => |i| switch (i) {
            .i1 => .integer1,
            .i2 => .integer2,
            .i4 => .integer4,
            .i8 => .integer8,
        },
        .packed_decimal => .decimal,
        .string => .alpha,
        .handle => .handle,
        .structure_ref, .class_ref => .structure,
    };
}

fn getSizeFromDataType(dt: ast.DataType) u16 {
    return switch (dt) {
        .alpha => |a| if (a.size) |s| @intCast(s) else 10,
        .decimal => |d| if (d.size) |s| @intCast(s) else 10,
        .implied_decimal => |id| @intCast(id.total_digits),
        .integer => |i| switch (i) {
            .i1 => 1,
            .i2 => 2,
            .i4 => 4,
            .i8 => 8,
        },
        .packed_decimal => |p| @intCast(p.size),
        .string => 10,
        .handle => 8,
        .structure_ref, .class_ref => 0,
    };
}

fn getPrecisionFromDataType(dt: ast.DataType) u8 {
    return switch (dt) {
        .implied_decimal => |id| @intCast(id.precision),
        .packed_decimal => |p| if (p.precision) |prec| @intCast(prec) else 0,
        else => 0,
    };
}

fn getBuiltinOpcode(name: []const u8) ?Opcode {
    const builtins = std.StaticStringMap(Opcode).initComptime(.{
        .{ "abs", .fn_abs },
        .{ "%abs", .fn_abs },
        .{ "sqrt", .fn_sqrt },
        .{ "%sqrt", .fn_sqrt },
        .{ "sin", .fn_sin },
        .{ "%sin", .fn_sin },
        .{ "cos", .fn_cos },
        .{ "%cos", .fn_cos },
        .{ "tan", .fn_tan },
        .{ "%tan", .fn_tan },
        .{ "log", .fn_log },
        .{ "%log", .fn_log },
        .{ "log10", .fn_log10 },
        .{ "%log10", .fn_log10 },
        .{ "exp", .fn_exp },
        .{ "%exp", .fn_exp },
        .{ "round", .fn_round },
        .{ "%round", .fn_round },
        .{ "trunc", .fn_trunc },
        .{ "%trunc", .fn_trunc },
        .{ "date", .fn_date },
        .{ "%date", .fn_date },
        .{ "time", .fn_time },
        .{ "%time", .fn_time },
        .{ "size", .fn_size },
        .{ "%size", .fn_size },
        .{ "instr", .fn_instr },
        .{ "%instr", .fn_instr },
        .{ "mem", .fn_mem },
        .{ "%mem", .fn_mem },
        .{ "error", .fn_error },
        .{ "%error", .fn_error },
        .{ "trim", .str_trim },
        .{ "%trim", .str_trim },
        .{ "atrim", .str_trim },
        .{ "%atrim", .str_trim },
        .{ "ltrim", .str_ltrim },
        .{ "%ltrim", .str_ltrim },
        .{ "upper", .str_upper },
        .{ "lower", .str_lower },
        .{ "len", .str_len },
        .{ "%len", .str_len },
    });
    return builtins.get(name);
}

test "compiler init" {
    const allocator = std.testing.allocator;
    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    try std.testing.expectEqual(@as(usize, 0), compiler.code.items.len);
    try std.testing.expectEqual(@as(usize, 0), compiler.constants.items.len);
}

test "emit opcodes" {
    const allocator = std.testing.allocator;
    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    try compiler.emit(.push_i8);
    try compiler.emitU8(42);
    try compiler.emit(.halt);

    try std.testing.expectEqual(@as(usize, 3), compiler.code.items.len);
    try std.testing.expectEqual(@as(u8, 0x04), compiler.code.items[0]); // push_i8
    try std.testing.expectEqual(@as(u8, 42), compiler.code.items[1]);
    try std.testing.expectEqual(@as(u8, 0xFF), compiler.code.items[2]); // halt
}

test "add constants" {
    const allocator = std.testing.allocator;
    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    const idx1 = try compiler.addStringConstant("hello");
    const idx2 = try compiler.addStringConstant("world");
    const idx3 = try compiler.addStringConstant("hello"); // Duplicate

    try std.testing.expectEqual(@as(u16, 0), idx1);
    try std.testing.expectEqual(@as(u16, 1), idx2);
    try std.testing.expectEqual(@as(u16, 0), idx3); // Same as first
}
