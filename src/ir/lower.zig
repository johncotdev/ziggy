//! Zibol IR Lowering
//!
//! Converts the Zibol AST into the IR representation.
//! This is the bridge between parsing and code generation.
//!
//! The lowering process:
//! 1. Collects record definitions and creates IR record types
//! 2. Creates IR functions from procedure definitions
//! 3. Lowers statements to IR instructions
//! 4. Lowers expressions to IR values

const std = @import("std");
const ast = @import("../ast/ast.zig");
const ir = @import("ir.zig");

const Allocator = std.mem.Allocator;

pub const LowerError = error{
    OutOfMemory,
    UndefinedVariable,
    UndefinedType,
    TypeMismatch,
    InvalidExpression,
    UnsupportedFeature,
};

/// Lowering context that tracks state during AST to IR conversion
pub const Lowerer = struct {
    allocator: Allocator,
    module: *ir.Module,
    current_func: ?*ir.Function,
    current_block: ?*ir.Block,

    /// Map variable names to their IR values (allocas)
    variables: std.StringHashMap(ir.Value),

    /// Map record names to their IR types
    record_types: std.StringHashMap(*const ir.RecordType),

    /// Map field names to their offsets within the current record context
    field_offsets: std.StringHashMap(FieldInfo),

    /// Map label names to their IR blocks
    labels: std.StringHashMap(*ir.Block),

    /// Pending goto statements that need label resolution
    pending_gotos: std.ArrayList(PendingGoto),

    /// Allocated Type pointers that need to be freed on deinit
    allocated_types: std.ArrayList(*ir.Type),

    /// Allocated Value slices that need to be freed on deinit
    allocated_value_slices: std.ArrayList([]const ir.Value),

    const FieldInfo = struct {
        ty: ir.Type,
        offset: u32,
        index: u32,
    };

    const PendingGoto = struct {
        from_block: *ir.Block,
        label_name: []const u8,
    };

    const Self = @This();

    pub fn init(allocator: Allocator, module_name: []const u8) !Self {
        const module = try allocator.create(ir.Module);
        module.* = ir.Module.init(allocator, module_name);

        return .{
            .allocator = allocator,
            .module = module,
            .current_func = null,
            .current_block = null,
            .variables = std.StringHashMap(ir.Value).init(allocator),
            .record_types = std.StringHashMap(*const ir.RecordType).init(allocator),
            .field_offsets = std.StringHashMap(FieldInfo).init(allocator),
            .labels = std.StringHashMap(*ir.Block).init(allocator),
            .pending_gotos = .{},
            .allocated_types = .{},
            .allocated_value_slices = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.variables.deinit();
        self.record_types.deinit();
        self.field_offsets.deinit();
        self.labels.deinit();
        self.pending_gotos.deinit(self.allocator);

        // Free allocated Type pointers
        for (self.allocated_types.items) |ty_ptr| {
            self.allocator.destroy(ty_ptr);
        }
        self.allocated_types.deinit(self.allocator);

        // Free allocated Value slices
        for (self.allocated_value_slices.items) |slice| {
            self.allocator.free(slice);
        }
        self.allocated_value_slices.deinit(self.allocator);
        // Note: module is returned to caller, not freed here
    }

    /// Lower an entire AST program to IR
    pub fn lowerProgram(self: *Self, program: *const ast.Program) LowerError!*ir.Module {
        // First pass: collect record definitions
        for (program.statements) |stmt| {
            switch (stmt) {
                .record => |rec| {
                    try self.lowerRecordDef(&rec);
                },
                else => {},
            }
        }

        // Second pass: lower function and subroutine definitions
        for (program.statements) |stmt| {
            switch (stmt) {
                .function_def => |func| try self.lowerFunctionDef(&func),
                .subroutine_def => |sub| try self.lowerSubroutineDef(&sub),
                else => {},
            }
        }

        // Third pass: create a main function for procedural code
        // In Zibol, code after PROC runs as the main entry point
        var in_proc = false;
        var proc_statements: std.ArrayList(ast.Statement) = .{};
        defer proc_statements.deinit(self.allocator);

        for (program.statements) |stmt| {
            switch (stmt) {
                .proc => {
                    in_proc = true;
                },
                .record, .group, .literal, .common, .function_def, .subroutine_def => {
                    // Data division or definitions - handled in earlier passes
                },
                else => {
                    if (in_proc) {
                        try proc_statements.append(self.allocator, stmt);
                    }
                },
            }
        }

        // Create main function if we have procedural statements
        if (proc_statements.items.len > 0) {
            try self.createMainFunction(proc_statements.items);
        }

        return self.module;
    }

    /// Lower a record definition to an IR record type
    fn lowerRecordDef(self: *Self, rec: *const ast.RecordDef) LowerError!void {
        var fields: std.ArrayList(ir.RecordType.Field) = .{};
        errdefer fields.deinit(self.allocator);

        var offset: u32 = 0;
        var field_index: u32 = 0;

        for (rec.fields) |field| {
            const ir_type = try self.lowerDataType(&field.data_type);
            const field_size = ir_type.sizeInBytes();

            try fields.append(self.allocator, .{
                .name = field.name,
                .ty = ir_type,
                .offset = offset,
            });

            // Track field info for later lookups
            try self.field_offsets.put(field.name, .{
                .ty = ir_type,
                .offset = offset,
                .index = field_index,
            });

            offset += field_size;
            field_index += 1;
        }

        const record_type = try self.allocator.create(ir.RecordType);
        record_type.* = .{
            .name = rec.name orelse "anonymous",
            .fields = fields.toOwnedSlice(self.allocator) catch return LowerError.OutOfMemory,
            .size = offset,
        };

        try self.module.records.append(self.allocator, record_type);

        if (rec.name) |name| {
            try self.record_types.put(name, record_type);

            // In DBL, a named record also creates a variable of that record type
            // Add it to field_offsets so it gets a variable in createMainFunction
            try self.field_offsets.put(name, .{
                .ty = ir.Type{ .record = record_type },
                .offset = 0, // Record variables don't have an offset in the parent
                .index = 0,
            });
        }
    }

    /// Create the main function from procedural statements
    fn createMainFunction(self: *Self, statements: []const ast.Statement) LowerError!void {
        const sig = ir.FunctionType{
            .params = &[_]ir.FunctionType.Param{},
            .return_type = .void,
            .is_variadic = false,
        };

        const func = ir.Function.init(self.allocator, "main", sig) catch return LowerError.OutOfMemory;
        self.current_func = func;
        self.current_block = func.entry;

        // Allocate space for all record fields as local variables
        var iter = self.field_offsets.iterator();
        while (iter.next()) |entry| {
            const name = entry.key_ptr.*;
            const info = entry.value_ptr.*;

            // Allocate the type on the heap for stable pointer
            const ty_ptr = try self.allocator.create(ir.Type);
            ty_ptr.* = info.ty;
            try self.allocated_types.append(self.allocator, ty_ptr);

            const alloca_result = func.newValue(.{ .ptr = ty_ptr });

            try self.emit(.{
                .alloca = .{
                    .ty = info.ty,
                    .name = name,
                    .result = alloca_result,
                },
            });

            try self.variables.put(name, alloca_result);
        }

        // Lower each statement
        for (statements) |stmt| {
            try self.lowerStatement(&stmt);
        }

        // Add return if block isn't terminated
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .ret = null });
        }

        // Resolve any forward goto references
        try self.resolvePendingGotos();

        try self.module.addFunction(func);
    }

    /// Lower a function definition to IR
    fn lowerFunctionDef(self: *Self, func_def: *const ast.FunctionDef) LowerError!void {
        // Build parameter list
        var params: std.ArrayList(ir.FunctionType.Param) = .{};
        defer params.deinit(self.allocator);

        for (func_def.parameters) |param| {
            const ir_type = try self.lowerDataType(&param.data_type);
            try params.append(self.allocator, .{
                .name = param.name,
                .ty = ir_type,
                .direction = .in, // Default to input parameter
            });
        }

        const return_type = try self.lowerDataType(&func_def.return_type);

        const sig = ir.FunctionType{
            .params = params.toOwnedSlice(self.allocator) catch return LowerError.OutOfMemory,
            .return_type = return_type,
            .is_variadic = false,
        };

        const func = ir.Function.init(self.allocator, func_def.name, sig) catch return LowerError.OutOfMemory;
        self.current_func = func;
        self.current_block = func.entry;

        // Set export if specified
        if (func_def.export_name) |exp_name| {
            func.setExport(exp_name);
        }

        // Clear variable map for this function
        self.variables.clearRetainingCapacity();

        // Create allocas for parameters
        for (func_def.parameters) |param| {
            const ir_type = try self.lowerDataType(&param.data_type);
            const alloca_result = func.newValue(.{ .ptr = &ir_type });

            try self.emit(.{
                .alloca = .{
                    .ty = ir_type,
                    .name = param.name,
                    .result = alloca_result,
                },
            });

            try self.variables.put(param.name, alloca_result);
        }

        // Clear labels for this function scope
        self.labels.clearRetainingCapacity();
        self.pending_gotos.clearRetainingCapacity();

        // Lower body statements
        for (func_def.body) |stmt| {
            try self.lowerStatement(&stmt);
        }

        // Add return if block isn't terminated
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .ret = null });
        }

        // Resolve any forward goto references
        try self.resolvePendingGotos();

        try self.module.addFunction(func);
    }

    /// Lower a subroutine definition to IR
    fn lowerSubroutineDef(self: *Self, sub_def: *const ast.SubroutineDef) LowerError!void {
        // Build parameter list
        var params: std.ArrayList(ir.FunctionType.Param) = .{};
        defer params.deinit(self.allocator);

        for (sub_def.parameters) |param| {
            const ir_type = try self.lowerDataType(&param.data_type);
            try params.append(self.allocator, .{
                .name = param.name,
                .ty = ir_type,
                .direction = .inout, // Subroutine params are typically inout
            });
        }

        const sig = ir.FunctionType{
            .params = params.toOwnedSlice(self.allocator) catch return LowerError.OutOfMemory,
            .return_type = .void,
            .is_variadic = false,
        };

        const func = ir.Function.init(self.allocator, sub_def.name, sig) catch return LowerError.OutOfMemory;
        self.current_func = func;
        self.current_block = func.entry;

        // Set export if specified
        if (sub_def.export_name) |exp_name| {
            func.setExport(exp_name);
        }

        // Clear variable map
        self.variables.clearRetainingCapacity();

        // Create allocas for parameters
        for (sub_def.parameters) |param| {
            const ir_type = try self.lowerDataType(&param.data_type);
            const alloca_result = func.newValue(.{ .ptr = &ir_type });

            try self.emit(.{
                .alloca = .{
                    .ty = ir_type,
                    .name = param.name,
                    .result = alloca_result,
                },
            });

            try self.variables.put(param.name, alloca_result);
        }

        // Clear labels for this subroutine scope
        self.labels.clearRetainingCapacity();
        self.pending_gotos.clearRetainingCapacity();

        // Lower body statements
        for (sub_def.body) |stmt| {
            try self.lowerStatement(&stmt);
        }

        // Add return if not terminated
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .ret = null });
        }

        // Resolve any forward goto references
        try self.resolvePendingGotos();

        try self.module.addFunction(func);
    }

    /// Lower a single statement to IR
    fn lowerStatement(self: *Self, stmt: *const ast.Statement) LowerError!void {
        switch (stmt.*) {
            .assignment => |a| try self.lowerAssignment(&a),
            .display_stmt => |d| try self.lowerDisplay(&d),
            .if_stmt => |i| try self.lowerIf(&i),
            .xcall => |x| try self.lowerXCall(&x),
            .return_stmt => |r| try self.lowerReturn(&r),
            .block => |b| {
                for (b.statements) |s| {
                    try self.lowerStatement(&s);
                }
            },
            .open_stmt => |o| try self.lowerOpen(&o),
            .close_stmt => |c| try self.lowerClose(&c),
            .read_stmt => |r| try self.lowerRead(&r),
            .write_stmt => |w| try self.lowerWrite(&w),
            .store_stmt => |s| try self.lowerStore(&s),
            .delete_stmt => |d| try self.lowerDelete(&d),
            .incr_stmt => |inc| try self.lowerIncr(&inc),
            .clear_stmt => |clr| try self.lowerClear(&clr),
            .init_stmt => |init_s| try self.lowerInit(&init_s),
            .expression => |e| {
                // Expression statement - evaluate for side effects
                _ = try self.lowerExpression(&e);
            },
            .label => |l| try self.lowerLabel(&l),
            .goto_stmt => |g| try self.lowerGoto(&g),
            .call => |c| try self.lowerCall(&c),
            .loop => |lp| try self.lowerLoop(&lp),
            .case_stmt => |cs| try self.lowerCase(&cs),
            else => {
                // Skip unsupported statements for now
            },
        }
    }

    /// Lower an assignment statement
    fn lowerAssignment(self: *Self, a: *const ast.Assignment) LowerError!void {
        const value = try self.lowerExpression(&a.value);

        // Get the target variable
        switch (a.target) {
            .identifier => |name| {
                if (self.variables.get(name)) |ptr| {
                    try self.emit(.{
                        .store = .{
                            .ptr = ptr,
                            .value = value,
                        },
                    });
                } else {
                    return LowerError.UndefinedVariable;
                }
            },
            else => {
                // TODO: Handle more complex LHS (array indexing, member access)
                return LowerError.UnsupportedFeature;
            },
        }
    }

    /// Lower a display statement
    fn lowerDisplay(self: *Self, d: *const ast.DisplayStatement) LowerError!void {
        const channel = try self.lowerExpression(&d.channel);

        var values: std.ArrayList(ir.Value) = .{};
        errdefer values.deinit(self.allocator);

        for (d.expressions) |expr| {
            const val = try self.lowerExpression(&expr);
            try values.append(self.allocator, val);
        }

        const values_slice = values.toOwnedSlice(self.allocator) catch return LowerError.OutOfMemory;
        try self.allocated_value_slices.append(self.allocator, values_slice);

        try self.emit(.{
            .io_display = .{
                .channel = channel,
                .values = values_slice,
            },
        });
    }

    /// Lower an if statement
    fn lowerIf(self: *Self, i: *const ast.IfStatement) LowerError!void {
        const func = self.current_func orelse return LowerError.UnsupportedFeature;
        const condition = try self.lowerExpression(i.condition);

        // Create blocks
        const then_block = func.createBlock("then") catch return LowerError.OutOfMemory;
        const else_block = if (i.else_branch != null)
            func.createBlock("else") catch return LowerError.OutOfMemory
        else
            null;
        const merge_block = func.createBlock("merge") catch return LowerError.OutOfMemory;

        // Conditional branch
        try self.emit(.{
            .cond_br = .{
                .condition = condition,
                .then_block = then_block,
                .else_block = else_block orelse merge_block,
            },
        });

        // Then block
        self.current_block = then_block;
        try self.lowerStatement(i.then_branch);
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .br = .{ .target = merge_block } });
        }

        // Else block
        if (i.else_branch) |eb| {
            self.current_block = else_block.?;
            try self.lowerStatement(eb);
            if (!self.current_block.?.isTerminated()) {
                try self.emit(.{ .br = .{ .target = merge_block } });
            }
        }

        // Continue in merge block
        self.current_block = merge_block;
    }

    /// Lower an xcall statement
    fn lowerXCall(self: *Self, x: *const ast.XCallStatement) LowerError!void {
        var args: std.ArrayList(ir.Value) = .{};
        errdefer args.deinit(self.allocator);

        for (x.arguments) |arg| {
            const val = try self.lowerExpression(&arg);
            try args.append(self.allocator, val);
        }

        const args_slice = args.toOwnedSlice(self.allocator) catch return LowerError.OutOfMemory;
        try self.allocated_value_slices.append(self.allocator, args_slice);

        try self.emit(.{
            .xcall = .{
                .routine = x.routine_name,
                .args = args_slice,
            },
        });
    }

    /// Lower a return statement
    fn lowerReturn(self: *Self, r: *const ast.ReturnStatement) LowerError!void {
        const value = if (r.value) |v|
            try self.lowerExpression(&v)
        else
            null;

        try self.emit(.{ .ret = value });
    }

    /// Lower I/O open statement
    fn lowerOpen(self: *Self, o: *const ast.OpenStatement) LowerError!void {
        const channel = try self.lowerExpression(&o.channel);
        const filename = try self.lowerExpression(&o.filename);

        try self.emit(.{
            .io_open = .{
                .channel = channel,
                .mode = o.mode,
                .filename = filename,
            },
        });
    }

    /// Lower I/O close statement
    fn lowerClose(self: *Self, c: *const ast.CloseStatement) LowerError!void {
        const channel = try self.lowerExpression(&c.channel);

        try self.emit(.{
            .io_close = .{
                .channel = channel,
            },
        });
    }

    /// Lower I/O read statement
    fn lowerRead(self: *Self, r: *const ast.ReadStatement) LowerError!void {
        const channel = try self.lowerExpression(&r.channel);
        const record = try self.lowerExpression(&r.record);

        const key = if (r.key) |k|
            try self.lowerExpression(&k)
        else
            null;

        try self.emit(.{
            .io_read = .{
                .channel = channel,
                .record = record,
                .key = key,
                .qualifiers = .{}, // TODO: Lower qualifiers
            },
        });
    }

    /// Lower I/O write statement
    fn lowerWrite(self: *Self, w: *const ast.WriteStatement) LowerError!void {
        const channel = try self.lowerExpression(&w.channel);
        const record = try self.lowerExpression(&w.record);

        try self.emit(.{
            .io_write = .{
                .channel = channel,
                .record = record,
            },
        });
    }

    /// Lower I/O store statement
    fn lowerStore(self: *Self, s: *const ast.StoreStatement) LowerError!void {
        const channel = try self.lowerExpression(&s.channel);
        const record = try self.lowerExpression(&s.record);

        try self.emit(.{
            .io_write = .{
                .channel = channel,
                .record = record,
            },
        });
    }

    /// Lower I/O delete statement
    fn lowerDelete(self: *Self, d: *const ast.DeleteStatement) LowerError!void {
        const channel = try self.lowerExpression(&d.channel);

        try self.emit(.{
            .io_close = .{
                .channel = channel,
            },
        });
    }

    /// Lower increment statement
    fn lowerIncr(self: *Self, inc: *const ast.IncrStatement) LowerError!void {
        const func = self.current_func orelse return LowerError.UnsupportedFeature;

        // Get target variable
        const target_name = switch (inc.target) {
            .identifier => |name| name,
            else => return LowerError.UnsupportedFeature,
        };

        const ptr = self.variables.get(target_name) orelse return LowerError.UndefinedVariable;

        // Load current value
        const current = func.newValue(ir.Type{ .decimal = .{ .length = 8, .decimal_places = 0 } });
        try self.emit(.{
            .load = .{
                .ptr = ptr,
                .result = current,
            },
        });

        // Get increment amount (default 1)
        const amount = if (inc.amount) |a|
            try self.lowerExpression(&a)
        else blk: {
            const one = func.newValue(ir.Type{ .decimal = .{ .length = 8, .decimal_places = 0 } });
            try self.emit(.{
                .const_int = .{
                    .ty = .{ .decimal = .{ .length = 8, .decimal_places = 0 } },
                    .value = 1,
                },
            });
            break :blk one;
        };

        // Add
        const result = func.newValue(current.ty);
        try self.emit(.{
            .add = .{
                .lhs = current,
                .rhs = amount,
                .result = result,
            },
        });

        // Store back
        try self.emit(.{
            .store = .{
                .ptr = ptr,
                .value = result,
            },
        });
    }

    /// Lower clear statement
    fn lowerClear(self: *Self, clr: *const ast.ClearStatement) LowerError!void {
        const func = self.current_func orelse return LowerError.UnsupportedFeature;

        const target_name = switch (clr.target) {
            .identifier => |name| name,
            else => return LowerError.UnsupportedFeature,
        };

        const ptr = self.variables.get(target_name) orelse return LowerError.UndefinedVariable;

        // Create zero value based on type
        const zero = func.newValue(ptr.ty);
        try self.emit(.{
            .const_int = .{
                .ty = ptr.ty,
                .value = 0,
            },
        });

        try self.emit(.{
            .store = .{
                .ptr = ptr,
                .value = zero,
            },
        });
    }

    /// Lower init statement (same as clear for now)
    fn lowerInit(self: *Self, init_stmt: *const ast.InitStatement) LowerError!void {
        const func = self.current_func orelse return LowerError.UnsupportedFeature;

        const target_name = switch (init_stmt.target) {
            .identifier => |name| name,
            else => return LowerError.UnsupportedFeature,
        };

        const ptr = self.variables.get(target_name) orelse return LowerError.UndefinedVariable;

        const zero = func.newValue(ptr.ty);
        try self.emit(.{
            .const_int = .{
                .ty = ptr.ty,
                .value = 0,
            },
        });

        try self.emit(.{
            .store = .{
                .ptr = ptr,
                .value = zero,
            },
        });
    }

    /// Lower a label definition - creates a new basic block
    fn lowerLabel(self: *Self, l: *const ast.LabelDef) LowerError!void {
        const func = self.current_func orelse return LowerError.UnsupportedFeature;

        // Create a new block for this label
        const label_block = func.createBlock(l.name) catch return LowerError.OutOfMemory;

        // If current block isn't terminated, branch to the label block
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .br = .{ .target = label_block } });
        }

        // Register the label
        try self.labels.put(l.name, label_block);

        // Continue emitting code in the label block
        self.current_block = label_block;
    }

    /// Lower a goto statement
    fn lowerGoto(self: *Self, g: *const ast.GotoStatement) LowerError!void {
        // Check if label is already defined
        if (self.labels.get(g.label)) |target_block| {
            // Label exists, emit direct branch
            try self.emit(.{ .br = .{ .target = target_block } });
        } else {
            // Forward reference - record for later resolution
            try self.pending_gotos.append(self.allocator, .{
                .from_block = self.current_block.?,
                .label_name = g.label,
            });
            // We'll need to patch this later when the label is defined
        }
    }

    /// Lower a call statement (internal subroutine call)
    fn lowerCall(self: *Self, c: *const ast.CallStatement) LowerError!void {
        // CALL jumps to a label and returns
        // For now, treat it like a goto (will need proper call/return semantics later)
        if (self.labels.get(c.label)) |target_block| {
            try self.emit(.{ .br = .{ .target = target_block } });
        } else {
            try self.pending_gotos.append(self.allocator, .{
                .from_block = self.current_block.?,
                .label_name = c.label,
            });
        }
    }

    /// Lower a loop statement
    fn lowerLoop(self: *Self, lp: *const ast.LoopStatement) LowerError!void {
        const func = self.current_func orelse return LowerError.UnsupportedFeature;

        // Create blocks for loop structure
        const cond_block = func.createBlock("loop_cond") catch return LowerError.OutOfMemory;
        const body_block = func.createBlock("loop_body") catch return LowerError.OutOfMemory;
        const exit_block = func.createBlock("loop_exit") catch return LowerError.OutOfMemory;

        // Handle initialization if present (FOR loops)
        if (lp.init_expr) |init_expr| {
            // If it's an assignment expression, evaluate it
            _ = try self.lowerExpression(&init_expr);
        }

        switch (lp.loop_type) {
            .do_forever => {
                // DO FOREVER - no condition, just loop
                try self.emit(.{ .br = .{ .target = body_block } });

                self.current_block = body_block;
                try self.lowerStatement(lp.body);
                if (!self.current_block.?.isTerminated()) {
                    try self.emit(.{ .br = .{ .target = body_block } });
                }

                self.current_block = exit_block;
            },
            .do_until => {
                // DO body UNTIL condition
                // Execute body first, then check condition
                try self.emit(.{ .br = .{ .target = body_block } });

                self.current_block = body_block;
                try self.lowerStatement(lp.body);
                if (!self.current_block.?.isTerminated()) {
                    try self.emit(.{ .br = .{ .target = cond_block } });
                }

                self.current_block = cond_block;
                if (lp.condition) |cond| {
                    const condition = try self.lowerExpression(&cond);
                    // Until means exit when true
                    try self.emit(.{
                        .cond_br = .{
                            .condition = condition,
                            .then_block = exit_block,
                            .else_block = body_block,
                        },
                    });
                }

                self.current_block = exit_block;
            },
            .while_loop => {
                // WHILE condition DO body
                try self.emit(.{ .br = .{ .target = cond_block } });

                self.current_block = cond_block;
                if (lp.condition) |cond| {
                    const condition = try self.lowerExpression(&cond);
                    try self.emit(.{
                        .cond_br = .{
                            .condition = condition,
                            .then_block = body_block,
                            .else_block = exit_block,
                        },
                    });
                }

                self.current_block = body_block;
                try self.lowerStatement(lp.body);
                if (!self.current_block.?.isTerminated()) {
                    try self.emit(.{ .br = .{ .target = cond_block } });
                }

                self.current_block = exit_block;
            },
            .for_from_thru, .for_do => {
                // FOR var FROM start THRU end [BY step]
                try self.emit(.{ .br = .{ .target = cond_block } });

                self.current_block = cond_block;
                if (lp.condition) |cond| {
                    const condition = try self.lowerExpression(&cond);
                    try self.emit(.{
                        .cond_br = .{
                            .condition = condition,
                            .then_block = body_block,
                            .else_block = exit_block,
                        },
                    });
                } else {
                    try self.emit(.{ .br = .{ .target = body_block } });
                }

                self.current_block = body_block;
                try self.lowerStatement(lp.body);

                // Update expression (increment/decrement)
                if (lp.update_expr) |update| {
                    _ = try self.lowerExpression(&update);
                }

                if (!self.current_block.?.isTerminated()) {
                    try self.emit(.{ .br = .{ .target = cond_block } });
                }

                self.current_block = exit_block;
            },
            .foreach => {
                // FOREACH - iterate over collection
                // For now, treat like a while loop with the condition
                try self.emit(.{ .br = .{ .target = cond_block } });

                self.current_block = cond_block;
                if (lp.condition) |cond| {
                    const condition = try self.lowerExpression(&cond);
                    try self.emit(.{
                        .cond_br = .{
                            .condition = condition,
                            .then_block = body_block,
                            .else_block = exit_block,
                        },
                    });
                }

                self.current_block = body_block;
                try self.lowerStatement(lp.body);
                if (!self.current_block.?.isTerminated()) {
                    try self.emit(.{ .br = .{ .target = cond_block } });
                }

                self.current_block = exit_block;
            },
        }
    }

    /// Lower a case statement (USING/CASE)
    fn lowerCase(self: *Self, cs: *const ast.CaseStatement) LowerError!void {
        const func = self.current_func orelse return LowerError.UnsupportedFeature;

        // Evaluate the selector expression
        const selector = try self.lowerExpression(&cs.selector);

        // Create exit block
        const exit_block = func.createBlock("case_exit") catch return LowerError.OutOfMemory;

        // Create blocks for each case
        var case_blocks: std.ArrayList(*ir.Block) = .{};
        defer case_blocks.deinit(self.allocator);

        for (cs.cases) |_| {
            const case_block = func.createBlock("case") catch return LowerError.OutOfMemory;
            try case_blocks.append(self.allocator, case_block);
        }

        // Create default block if present
        const default_block = if (cs.default_branch != null)
            func.createBlock("case_default") catch return LowerError.OutOfMemory
        else
            exit_block;

        // Emit comparison chain
        for (cs.cases, 0..) |case_branch, i| {
            for (case_branch.values) |val| {
                const case_val = try self.lowerExpression(&val);

                // Compare selector with case value
                const cmp_result = func.newValue(.{ .integer = 1 });
                try self.emit(.{
                    .cmp_eq = .{
                        .lhs = selector,
                        .rhs = case_val,
                        .result = cmp_result,
                    },
                });

                // Create next check block
                const next_check = if (i + 1 < cs.cases.len)
                    func.createBlock("case_check") catch return LowerError.OutOfMemory
                else
                    default_block;

                try self.emit(.{
                    .cond_br = .{
                        .condition = cmp_result,
                        .then_block = case_blocks.items[i],
                        .else_block = next_check,
                    },
                });

                self.current_block = next_check;
            }
        }

        // Emit case bodies
        for (cs.cases, 0..) |case_branch, i| {
            self.current_block = case_blocks.items[i];
            try self.lowerStatement(case_branch.body);
            if (!self.current_block.?.isTerminated()) {
                try self.emit(.{ .br = .{ .target = exit_block } });
            }
        }

        // Emit default body if present
        if (cs.default_branch) |default_stmt| {
            self.current_block = default_block;
            try self.lowerStatement(default_stmt);
            if (!self.current_block.?.isTerminated()) {
                try self.emit(.{ .br = .{ .target = exit_block } });
            }
        }

        self.current_block = exit_block;
    }

    /// Resolve any pending goto statements after all labels are defined
    fn resolvePendingGotos(self: *Self) LowerError!void {
        for (self.pending_gotos.items) |pending| {
            if (self.labels.get(pending.label_name)) |target_block| {
                // Patch the branch in the from_block
                pending.from_block.append(.{ .br = .{ .target = target_block } }) catch return LowerError.OutOfMemory;
            } else {
                // Undefined label - error
                return LowerError.UndefinedVariable;
            }
        }
        self.pending_gotos.clearRetainingCapacity();
    }

    /// Lower an expression to an IR value
    fn lowerExpression(self: *Self, expr: *const ast.Expression) LowerError!ir.Value {
        const func = self.current_func orelse return LowerError.UnsupportedFeature;

        switch (expr.*) {
            .integer => |val| {
                const result = func.newValue(.{ .decimal = .{ .length = 8, .decimal_places = 0 } });
                try self.emit(.{
                    .const_int = .{
                        .ty = .{ .decimal = .{ .length = 8, .decimal_places = 0 } },
                        .value = val,
                    },
                });
                return result;
            },
            .decimal => |val| {
                // Parse decimal string to i64
                const num = std.fmt.parseFloat(f64, val) catch 0.0;
                const scaled = @as(i64, @intFromFloat(num * 100)); // Assume 2 decimal places
                const result = func.newValue(.{ .decimal = .{ .length = 10, .decimal_places = 2 } });
                try self.emit(.{
                    .const_decimal = .{
                        .ty = .{ .decimal = .{ .length = 10, .decimal_places = 2 } },
                        .value = scaled,
                    },
                });
                return result;
            },
            .string => |val| {
                const result = func.newValue(.{ .alpha = @intCast(val.len) });
                try self.emit(.{
                    .const_alpha = .{
                        .value = val,
                    },
                });
                return result;
            },
            .identifier => |name| {
                if (self.variables.get(name)) |ptr| {
                    // Load the variable (includes named records which are added to variables)
                    const field_info = self.field_offsets.get(name);
                    const ty = if (field_info) |info| info.ty else ptr.ty;
                    const result = func.newValue(ty);
                    try self.emit(.{
                        .load = .{
                            .ptr = ptr,
                            .result = result,
                        },
                    });
                    return result;
                }
                return LowerError.UndefinedVariable;
            },
            .binary => |bin| {
                const lhs = try self.lowerExpression(&bin.left);
                const rhs = try self.lowerExpression(&bin.right);
                const result = func.newValue(lhs.ty);

                const op: ir.Instruction = switch (bin.operator) {
                    .add => .{ .add = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .subtract => .{ .sub = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .multiply => .{ .mul = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .divide => .{ .div = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .modulo => .{ .mod = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .equal => .{ .cmp_eq = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .not_equal => .{ .cmp_ne = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .less_than => .{ .cmp_lt = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .less_equal => .{ .cmp_le = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .greater_than => .{ .cmp_gt = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .greater_equal => .{ .cmp_ge = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .logical_and => .{ .log_and = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .logical_or => .{ .log_or = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    .concat => .{ .str_concat = .{ .lhs = lhs, .rhs = rhs, .result = result } },
                    else => return LowerError.UnsupportedFeature,
                };

                try self.emit(op);
                return result;
            },
            .unary => |un| {
                const operand = try self.lowerExpression(&un.operand);
                const result = func.newValue(operand.ty);

                const op: ir.Instruction = switch (un.operator) {
                    .negate => .{ .neg = .{ .operand = operand, .result = result } },
                    .logical_not => .{ .log_not = .{ .operand = operand, .result = result } },
                    else => return LowerError.UnsupportedFeature,
                };

                try self.emit(op);
                return result;
            },
            .grouping => |g| {
                return self.lowerExpression(g);
            },
            .call => |c| {
                var args: std.ArrayList(ir.Value) = .{};
                defer args.deinit(self.allocator);

                for (c.arguments) |arg| {
                    const val = try self.lowerExpression(&arg);
                    try args.append(self.allocator, val);
                }

                const result = func.newValue(.void);
                try self.emit(.{
                    .call = .{
                        .callee = c.callee,
                        .args = args.toOwnedSlice(self.allocator) catch return LowerError.OutOfMemory,
                        .result = result,
                    },
                });
                return result;
            },
            else => return LowerError.UnsupportedFeature,
        }
    }

    /// Convert AST DataType to IR Type
    fn lowerDataType(self: *Self, dt: *const ast.DataType) LowerError!ir.Type {
        _ = self;
        return switch (dt.*) {
            .alpha => |a| ir.Type{ .alpha = @intCast(a.size orelse 1) },
            .decimal => |d| ir.Type{ .decimal = .{
                .length = @intCast(d.size orelse 8),
                .decimal_places = 0,
            } },
            .implied_decimal => |id| ir.Type{ .decimal = .{
                .length = @intCast(id.total_digits),
                .decimal_places = @intCast(id.precision),
            } },
            .integer => |i| ir.Type{ .integer = switch (i) {
                .i1 => 1,
                .i2 => 2,
                .i4 => 4,
                .i8 => 8,
            } },
            .packed_decimal => |p| ir.Type{ .packed_decimal = .{
                .length = @intCast(p.size),
                .decimal_places = @intCast(p.precision orelse 0),
            } },
            .string => ir.Type{ .alpha = 0 }, // Dynamic string
            .handle => ir.Type{ .ptr = &ir.Type{ .void = {} } },
            .structure_ref, .class_ref => LowerError.UnsupportedFeature,
        };
    }

    /// Emit an instruction to the current block
    fn emit(self: *Self, inst: ir.Instruction) LowerError!void {
        const block = self.current_block orelse return LowerError.UnsupportedFeature;
        block.append(inst) catch return LowerError.OutOfMemory;
    }
};

// ============================================================================
// Public API
// ============================================================================

/// Lower an AST program to an IR module
pub fn lower(allocator: Allocator, program: *const ast.Program, module_name: []const u8) LowerError!*ir.Module {
    var lowerer = try Lowerer.init(allocator, module_name);
    defer lowerer.deinit();
    return lowerer.lowerProgram(program);
}

// ============================================================================
// Tests
// ============================================================================

test "lower simple record" {
    const allocator = std.testing.allocator;

    // Create a simple program with a record
    const fields = [_]ast.FieldDef{
        .{
            .name = "counter",
            .data_type = .{ .decimal = .{ .size = 4 } },
            .initial_value = null,
            .array_dims = null,
        },
    };

    const statements = [_]ast.Statement{
        .{
            .record = .{
                .name = "test",
                .fields = &fields,
            },
        },
    };

    const program = ast.Program{
        .statements = @constCast(&statements),
        .allocator = allocator,
    };

    const module = try lower(allocator, &program, "test_module");
    defer module.deinit();

    try std.testing.expectEqual(@as(usize, 1), module.records.items.len);
    try std.testing.expectEqualStrings("test", module.records.items[0].name);
}

test "lower record with multiple fields" {
    const allocator = std.testing.allocator;

    const fields = [_]ast.FieldDef{
        .{
            .name = "id",
            .data_type = .{ .alpha = .{ .size = 8 } },
            .initial_value = null,
            .array_dims = null,
        },
        .{
            .name = "amount",
            .data_type = .{ .implied_decimal = .{ .total_digits = 10, .precision = 2 } },
            .initial_value = null,
            .array_dims = null,
        },
        .{
            .name = "count",
            .data_type = .{ .integer = .i4 },
            .initial_value = null,
            .array_dims = null,
        },
    };

    const statements = [_]ast.Statement{
        .{
            .record = .{
                .name = "order",
                .fields = &fields,
            },
        },
    };

    const program = ast.Program{
        .statements = @constCast(&statements),
        .allocator = allocator,
    };

    const module = try lower(allocator, &program, "order_module");
    defer module.deinit();

    try std.testing.expectEqual(@as(usize, 1), module.records.items.len);

    const rec = module.records.items[0];
    try std.testing.expectEqual(@as(usize, 3), rec.fields.len);
    try std.testing.expectEqual(@as(u32, 8 + 10 + 4), rec.size);
}
