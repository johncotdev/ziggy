//! Zibol IR to Bytecode Emitter
//!
//! Transforms IR into bytecode that can be executed by the Ziggy VM.
//! This replaces the direct AST-to-bytecode compilation path.

const std = @import("std");
const ir = @import("ir.zig");
const module = @import("../bytecode/module.zig");
const opcodes = @import("../bytecode/opcodes.zig");

const Allocator = std.mem.Allocator;
const Opcode = opcodes.Opcode;

/// Bytecode emitter errors
pub const EmitError = error{
    OutOfMemory,
    TooManyConstants,
    TooManyLocals,
    TooManyGlobals,
    JumpTooFar,
    InvalidInstruction,
    UndefinedBlock,
    StackOverflow,
};

/// Pending jump that needs to be patched
const PendingJump = struct {
    /// Offset in code where the jump offset should be written
    patch_offset: u32,
    /// Target block
    target_block: *const ir.Block,
    /// Whether this is a wide jump (i32) or normal (i16)
    is_wide: bool,
};

/// Variable location information
const VarLocation = struct {
    slot: u16,
    is_global: bool,
};

/// Bytecode emitter
pub const BytecodeEmitter = struct {
    allocator: Allocator,

    // Output buffers
    code: std.ArrayList(u8),
    constants: std.ArrayList(module.Constant),
    types: std.ArrayList(module.TypeDef),
    routines: std.ArrayList(module.RoutineDef),

    // Symbol tables
    globals: std.StringHashMap(VarLocation),
    locals: std.StringHashMap(VarLocation),
    global_count: u16,
    local_count: u16,

    // Constant deduplication
    string_pool: std.StringHashMap(u16),
    int_pool: std.AutoHashMap(i64, u16),

    // Stack tracking
    current_stack: u16,
    max_stack: u16,

    // Block management
    block_offsets: std.AutoHashMap(*const ir.Block, u32), // block -> code_offset
    pending_jumps: std.ArrayList(PendingJump),

    // IR value to stack slot mapping
    value_slots: std.AutoHashMap(u32, u16), // value_id -> stack_slot

    // Current routine being emitted
    current_routine_start: u32,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .code = .{},
            .constants = .{},
            .types = .{},
            .routines = .{},
            .globals = std.StringHashMap(VarLocation).init(allocator),
            .locals = std.StringHashMap(VarLocation).init(allocator),
            .global_count = 0,
            .local_count = 0,
            .string_pool = std.StringHashMap(u16).init(allocator),
            .int_pool = std.AutoHashMap(i64, u16).init(allocator),
            .current_stack = 0,
            .max_stack = 0,
            .block_offsets = std.AutoHashMap(*const ir.Block, u32).init(allocator),
            .pending_jumps = .{},
            .value_slots = std.AutoHashMap(u32, u16).init(allocator),
            .current_routine_start = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.types.deinit(self.allocator);
        self.routines.deinit(self.allocator);
        self.globals.deinit();
        self.locals.deinit();
        self.string_pool.deinit();
        self.int_pool.deinit();
        self.block_offsets.deinit();
        self.pending_jumps.deinit(self.allocator);
        self.value_slots.deinit();
    }

    /// Emit bytecode module from IR module
    pub fn emit(self: *Self, ir_module: *const ir.Module) EmitError!module.Module {
        // First pass: emit record types
        for (ir_module.records.items) |record| {
            try self.emitRecordType(record);
        }

        // Second pass: emit all functions
        for (ir_module.functions.items) |func| {
            try self.emitFunction(func);
        }

        // Emit halt at end
        try self.emitOpcode(.halt);

        // Build the module
        var result = module.Module{
            .allocator = self.allocator,
            .header = module.ModuleHeader.init(),
            .constants = try self.constants.toOwnedSlice(self.allocator),
            .types = try self.types.toOwnedSlice(self.allocator),
            .routines = try self.routines.toOwnedSlice(self.allocator),
            .code = try self.code.toOwnedSlice(self.allocator),
            .exports = &[_]module.ExportEntry{},
            .imports = &[_]module.ImportEntry{},
            .source_file = null,
            .line_table = null,
            .local_vars = null,
        };

        // Set entry point (offset of main function, or 0)
        result.header.entry_point = 0;

        return result;
    }

    /// Emit a record type definition
    fn emitRecordType(self: *Self, record: *const ir.RecordType) EmitError!void {
        var fields: std.ArrayList(module.FieldDef) = .{};
        errdefer fields.deinit(self.allocator);

        for (record.fields, 0..) |field, i| {
            const name_idx = try self.addIdentifier(field.name);
            try fields.append(self.allocator, .{
                .name_index = name_idx,
                .data_type = irTypeToDataType(field.ty),
                .flags = 0,
                .offset = @intCast(field.offset),
                .size = @intCast(field.ty.sizeInBytes()),
                .precision = if (field.ty == .decimal) field.ty.decimal.decimal_places else 0,
                .array_dims = &[_]u16{},
            });

            // Also register as a global variable
            try self.globals.put(field.name, .{
                .slot = self.global_count,
                .is_global = true,
            });
            self.global_count += 1;
            _ = i;
        }

        const name_idx = try self.addIdentifier(record.name);
        try self.types.append(self.allocator, .{
            .type_id = @intCast(self.types.items.len),
            .kind = .record,
            .flags = 0,
            .name_index = name_idx,
            .total_size = record.size,
            .fields = try fields.toOwnedSlice(self.allocator),
        });
    }

    /// Emit a function
    fn emitFunction(self: *Self, func: *const ir.Function) EmitError!void {
        // Clear per-function state
        self.locals.clearRetainingCapacity();
        self.local_count = 0;
        self.current_stack = 0;
        self.max_stack = 0;
        self.block_offsets.clearRetainingCapacity();
        self.pending_jumps.clearRetainingCapacity();
        self.value_slots.clearRetainingCapacity();

        self.current_routine_start = @intCast(self.code.items.len);

        // Allocate locals for parameters
        for (func.signature.params) |param| {
            try self.locals.put(param.name, .{
                .slot = self.local_count,
                .is_global = false,
            });
            self.local_count += 1;
        }

        // First pass: record block offsets
        // We need to know where each block starts before emitting jumps
        var estimated_offset: u32 = @intCast(self.code.items.len);
        for (func.blocks.items) |block| {
            try self.block_offsets.put(block, estimated_offset);
            // Rough estimate of block size (will be adjusted)
            estimated_offset += @intCast(block.instructions.items.len * 4);
        }

        // Second pass: emit blocks
        for (func.blocks.items) |block| {
            try self.emitBlock(block);
        }

        // Patch pending jumps
        try self.patchJumps();

        // Record routine definition
        const name_idx = try self.addIdentifier(func.name);
        const code_len = @as(u32, @intCast(self.code.items.len)) - self.current_routine_start;

        try self.routines.append(self.allocator, .{
            .name_index = name_idx,
            .flags = if (func.linkage == .external) module.RoutineFlags{ .is_public = true } else module.RoutineFlags{},
            .code_offset = self.current_routine_start,
            .code_length = code_len,
            .param_count = @intCast(func.signature.params.len),
            .local_count = self.local_count,
            .max_stack = self.max_stack,
            .params = &[_]module.ParamDef{}, // TODO: populate if needed
        });
    }

    /// Emit a basic block
    fn emitBlock(self: *Self, block: *const ir.Block) EmitError!void {
        // Update actual block offset
        try self.block_offsets.put(block, @intCast(self.code.items.len));

        for (block.instructions.items) |inst| {
            try self.emitInstruction(&inst);
        }
    }

    /// Emit a single IR instruction as bytecode
    fn emitInstruction(self: *Self, inst: *const ir.Instruction) EmitError!void {
        switch (inst.*) {
            .alloca => |a| {
                // Check if this is a record field (already registered as global)
                if (self.globals.get(a.name)) |global_info| {
                    // Use the existing global slot
                    try self.value_slots.put(a.result.id, global_info.slot | 0x8000); // Set high bit to mark as global
                } else {
                    // Allocate a local variable slot
                    try self.locals.put(a.name, .{
                        .slot = self.local_count,
                        .is_global = false,
                    });
                    try self.value_slots.put(a.result.id, self.local_count);
                    self.local_count += 1;
                }
            },

            .load => |l| {
                // Load value from memory location
                if (self.value_slots.get(l.ptr.id)) |slot_info| {
                    if (slot_info & 0x8000 != 0) {
                        // Global variable
                        const slot = slot_info & 0x7FFF;
                        try self.emitLoadGlobal(slot);
                    } else {
                        // Local variable
                        try self.emitLoadLocal(slot_info);
                    }
                    self.pushStack(1);
                    try self.value_slots.put(l.result.id, self.current_stack - 1);
                }
            },

            .store => |s| {
                // First, get the value on the stack
                try self.emitValue(s.value);

                // Store to the target
                if (self.value_slots.get(s.ptr.id)) |slot_info| {
                    if (slot_info & 0x8000 != 0) {
                        // Global variable
                        const slot = slot_info & 0x7FFF;
                        try self.emitStoreGlobal(slot);
                    } else {
                        // Local variable
                        try self.emitStoreLocal(slot_info);
                    }
                    self.popStack(1);
                }
            },

            .const_int => |c| {
                try self.emitPushInt(c.value);
                self.pushStack(1);
            },

            .const_decimal => |c| {
                const const_idx = try self.addConstant(.{
                    .decimal = .{
                        .value = c.value,
                        .precision = if (c.ty == .decimal) c.ty.decimal.decimal_places else 0,
                    },
                });
                try self.emitOpcode(.push_const);
                try self.emitU16(const_idx);
                self.pushStack(1);
            },

            .const_alpha => |c| {
                const const_idx = try self.addString(c.value);
                try self.emitOpcode(.push_const);
                try self.emitU16(const_idx);
                self.pushStack(1);
            },

            .add => |a| {
                try self.emitValue(a.lhs);
                try self.emitValue(a.rhs);
                if (a.lhs.ty == .decimal) {
                    try self.emitOpcode(.add_dec);
                    try self.emitU8(a.lhs.ty.decimal.decimal_places);
                } else {
                    try self.emitOpcode(.add);
                }
                self.popStack(1); // Two values -> one result
                try self.value_slots.put(a.result.id, self.current_stack - 1);
            },

            .sub => |s| {
                try self.emitValue(s.lhs);
                try self.emitValue(s.rhs);
                if (s.lhs.ty == .decimal) {
                    try self.emitOpcode(.sub_dec);
                } else {
                    try self.emitOpcode(.sub);
                }
                self.popStack(1);
                try self.value_slots.put(s.result.id, self.current_stack - 1);
            },

            .mul => |m| {
                try self.emitValue(m.lhs);
                try self.emitValue(m.rhs);
                if (m.lhs.ty == .decimal) {
                    try self.emitOpcode(.mul_dec);
                } else {
                    try self.emitOpcode(.mul);
                }
                self.popStack(1);
                try self.value_slots.put(m.result.id, self.current_stack - 1);
            },

            .div => |d| {
                try self.emitValue(d.lhs);
                try self.emitValue(d.rhs);
                if (d.lhs.ty == .decimal) {
                    try self.emitOpcode(.div_dec);
                } else {
                    try self.emitOpcode(.div);
                }
                self.popStack(1);
                try self.value_slots.put(d.result.id, self.current_stack - 1);
            },

            .mod => |m| {
                try self.emitValue(m.lhs);
                try self.emitValue(m.rhs);
                try self.emitOpcode(.mod);
                self.popStack(1);
                try self.value_slots.put(m.result.id, self.current_stack - 1);
            },

            .neg => |n| {
                try self.emitValue(n.operand);
                try self.emitOpcode(.neg);
                try self.value_slots.put(n.result.id, self.current_stack - 1);
            },

            .cmp_eq => |c| {
                try self.emitValue(c.lhs);
                try self.emitValue(c.rhs);
                if (c.lhs.ty == .alpha) {
                    try self.emitOpcode(.cmp_str_eq);
                } else {
                    try self.emitOpcode(.cmp_eq);
                }
                self.popStack(1);
                try self.value_slots.put(c.result.id, self.current_stack - 1);
            },

            .cmp_ne => |c| {
                try self.emitValue(c.lhs);
                try self.emitValue(c.rhs);
                try self.emitOpcode(.cmp_ne);
                self.popStack(1);
                try self.value_slots.put(c.result.id, self.current_stack - 1);
            },

            .cmp_lt => |c| {
                try self.emitValue(c.lhs);
                try self.emitValue(c.rhs);
                if (c.lhs.ty == .alpha) {
                    try self.emitOpcode(.cmp_str_lt);
                } else {
                    try self.emitOpcode(.cmp_lt);
                }
                self.popStack(1);
                try self.value_slots.put(c.result.id, self.current_stack - 1);
            },

            .cmp_le => |c| {
                try self.emitValue(c.lhs);
                try self.emitValue(c.rhs);
                try self.emitOpcode(.cmp_le);
                self.popStack(1);
                try self.value_slots.put(c.result.id, self.current_stack - 1);
            },

            .cmp_gt => |c| {
                try self.emitValue(c.lhs);
                try self.emitValue(c.rhs);
                try self.emitOpcode(.cmp_gt);
                self.popStack(1);
                try self.value_slots.put(c.result.id, self.current_stack - 1);
            },

            .cmp_ge => |c| {
                try self.emitValue(c.lhs);
                try self.emitValue(c.rhs);
                try self.emitOpcode(.cmp_ge);
                self.popStack(1);
                try self.value_slots.put(c.result.id, self.current_stack - 1);
            },

            .log_and => |l| {
                try self.emitValue(l.lhs);
                try self.emitValue(l.rhs);
                try self.emitOpcode(.log_and);
                self.popStack(1);
                try self.value_slots.put(l.result.id, self.current_stack - 1);
            },

            .log_or => |l| {
                try self.emitValue(l.lhs);
                try self.emitValue(l.rhs);
                try self.emitOpcode(.log_or);
                self.popStack(1);
                try self.value_slots.put(l.result.id, self.current_stack - 1);
            },

            .log_not => |l| {
                try self.emitValue(l.operand);
                try self.emitOpcode(.log_not);
                try self.value_slots.put(l.result.id, self.current_stack - 1);
            },

            .str_concat => |s| {
                try self.emitValue(s.lhs);
                try self.emitValue(s.rhs);
                try self.emitOpcode(.str_concat);
                self.popStack(1);
                try self.value_slots.put(s.result.id, self.current_stack - 1);
            },

            .br => |b| {
                // Unconditional branch
                try self.emitJump(b.target);
            },

            .cond_br => |c| {
                // Conditional branch
                try self.emitValue(c.condition);
                try self.emitOpcode(.jump_if_false);
                try self.addPendingJump(c.else_block, false);
                try self.emitI16(0); // Placeholder
                self.popStack(1);

                // Fall through to then block, or jump if not next
                try self.emitJump(c.then_block);
            },

            .ret => |r| {
                if (r) |val| {
                    try self.emitValue(val);
                    try self.emitOpcode(.ret_val);
                } else {
                    try self.emitOpcode(.ret);
                }
            },

            .call => |c| {
                // Push arguments
                for (c.args) |arg| {
                    try self.emitValue(arg);
                }
                // Find routine index
                const name_idx = try self.addIdentifier(c.callee);
                try self.emitOpcode(.call);
                try self.emitU16(name_idx);
                self.popStack(@intCast(c.args.len));
                if (c.result) |result| {
                    if (result.ty != .void) {
                        self.pushStack(1);
                        try self.value_slots.put(result.id, self.current_stack - 1);
                    }
                }
            },

            .xcall => |x| {
                // Push arguments
                for (x.args) |arg| {
                    try self.emitValue(arg);
                }
                const name_idx = try self.addIdentifier(x.routine);
                try self.emitOpcode(.xcall);
                try self.emitU16(name_idx);
                try self.emitU8(@intCast(x.args.len));
                self.popStack(@intCast(x.args.len));
            },

            .io_display => |d| {
                try self.emitValue(d.channel);
                for (d.values) |val| {
                    try self.emitValue(val);
                }
                try self.emitOpcode(.ch_display);
                try self.emitU8(@intCast(d.values.len));
                self.popStack(@intCast(d.values.len + 1));
            },

            .io_open => |o| {
                try self.emitValue(o.channel);
                try self.emitValue(o.filename);
                try self.emitOpcode(.ch_open);
                // Mode flags would go here
                try self.emitU8(0); // TODO: parse mode string
                self.popStack(2);
            },

            .io_close => |c| {
                try self.emitValue(c.channel);
                try self.emitOpcode(.ch_close);
                self.popStack(1);
            },

            .io_read => |r| {
                try self.emitValue(r.channel);
                try self.emitValue(r.record);
                if (r.key) |key| {
                    try self.emitValue(key);
                    try self.emitOpcode(.isam_read);
                    try self.emitU8(0); // key_num
                    try self.emitU8(0); // match_mode
                    self.popStack(3);
                } else {
                    try self.emitOpcode(.ch_read);
                    self.popStack(2);
                }
            },

            .io_write => |w| {
                try self.emitValue(w.channel);
                try self.emitValue(w.record);
                try self.emitOpcode(.ch_write);
                self.popStack(2);
            },

            else => {
                // Other instructions not yet implemented
            },
        }
    }

    /// Emit a value reference (load it onto the stack if needed)
    fn emitValue(self: *Self, value: ir.Value) EmitError!void {
        // Check if value is already on stack at known position
        if (self.value_slots.get(value.id)) |_| {
            // Value is in a local slot, need to load it
            // For now, we assume values flow directly
            return;
        }
        // Otherwise the value should already be on the stack from its definition
    }

    /// Emit opcode byte
    fn emitOpcode(self: *Self, op: Opcode) EmitError!void {
        try self.code.append(self.allocator, @intFromEnum(op));
    }

    /// Emit unsigned 8-bit value
    fn emitU8(self: *Self, val: u8) EmitError!void {
        try self.code.append(self.allocator, val);
    }

    /// Emit unsigned 16-bit value (little-endian)
    fn emitU16(self: *Self, val: u16) EmitError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    /// Emit signed 16-bit value (little-endian)
    fn emitI16(self: *Self, val: i16) EmitError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    /// Emit unsigned 32-bit value (little-endian)
    fn emitU32(self: *Self, val: u32) EmitError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    /// Emit signed 32-bit value (little-endian)
    fn emitI32(self: *Self, val: i32) EmitError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    /// Emit load local instruction with optimizations
    fn emitLoadLocal(self: *Self, slot: u16) EmitError!void {
        if (slot < 4) {
            try self.emitOpcode(switch (slot) {
                0 => .load_local_0,
                1 => .load_local_1,
                2 => .load_local_2,
                3 => .load_local_3,
                else => unreachable,
            });
        } else if (slot < 256) {
            try self.emitOpcode(.load_local);
            try self.emitU8(@intCast(slot));
        } else {
            try self.emitOpcode(.load_local_wide);
            try self.emitU16(slot);
        }
    }

    /// Emit store local instruction with optimizations
    fn emitStoreLocal(self: *Self, slot: u16) EmitError!void {
        if (slot < 4) {
            try self.emitOpcode(switch (slot) {
                0 => .store_local_0,
                1 => .store_local_1,
                2 => .store_local_2,
                3 => .store_local_3,
                else => unreachable,
            });
        } else if (slot < 256) {
            try self.emitOpcode(.store_local);
            try self.emitU8(@intCast(slot));
        } else {
            try self.emitOpcode(.store_local_wide);
            try self.emitU16(slot);
        }
    }

    /// Emit load global instruction
    fn emitLoadGlobal(self: *Self, slot: u16) EmitError!void {
        try self.emitOpcode(.load_global);
        try self.emitU16(slot);
    }

    /// Emit store global instruction
    fn emitStoreGlobal(self: *Self, slot: u16) EmitError!void {
        try self.emitOpcode(.store_global);
        try self.emitU16(slot);
    }

    /// Emit push integer with size optimization
    fn emitPushInt(self: *Self, val: i64) EmitError!void {
        if (val >= -128 and val <= 127) {
            try self.emitOpcode(.push_i8);
            try self.emitU8(@bitCast(@as(i8, @intCast(val))));
        } else if (val >= -32768 and val <= 32767) {
            try self.emitOpcode(.push_i16);
            try self.emitI16(@intCast(val));
        } else if (val >= -2147483648 and val <= 2147483647) {
            try self.emitOpcode(.push_i32);
            try self.emitI32(@intCast(val));
        } else {
            try self.emitOpcode(.push_i64);
            try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
        }
    }

    /// Emit jump to block
    fn emitJump(self: *Self, target: *const ir.Block) EmitError!void {
        try self.emitOpcode(.jump);
        try self.addPendingJump(target, false);
        try self.emitI16(0); // Placeholder
    }

    /// Add pending jump for later patching
    fn addPendingJump(self: *Self, target_block: *const ir.Block, is_wide: bool) EmitError!void {
        try self.pending_jumps.append(self.allocator, .{
            .patch_offset = @intCast(self.code.items.len),
            .target_block = target_block,
            .is_wide = is_wide,
        });
    }

    /// Patch all pending jumps with actual offsets
    fn patchJumps(self: *Self) EmitError!void {
        for (self.pending_jumps.items) |jump| {
            const target_offset = self.block_offsets.get(jump.target_block) orelse
                return EmitError.UndefinedBlock;

            const current_offset = jump.patch_offset + 2; // After the offset bytes
            const relative_offset = @as(i32, @intCast(target_offset)) - @as(i32, @intCast(current_offset));

            if (jump.is_wide) {
                std.mem.writeInt(i32, self.code.items[jump.patch_offset..][0..4], relative_offset, .little);
            } else {
                if (relative_offset > 32767 or relative_offset < -32768) {
                    return EmitError.JumpTooFar;
                }
                std.mem.writeInt(i16, self.code.items[jump.patch_offset..][0..2], @intCast(relative_offset), .little);
            }
        }
    }

    /// Add constant to pool, returning index
    fn addConstant(self: *Self, constant: module.Constant) EmitError!u16 {
        const idx = self.constants.items.len;
        if (idx > 65535) return EmitError.TooManyConstants;
        try self.constants.append(self.allocator, constant);
        return @intCast(idx);
    }

    /// Add string constant with deduplication
    fn addString(self: *Self, str: []const u8) EmitError!u16 {
        if (self.string_pool.get(str)) |idx| {
            return idx;
        }
        // Duplicate the string so the module owns it
        const owned_str = self.allocator.dupe(u8, str) catch return EmitError.OutOfMemory;
        const idx = try self.addConstant(.{ .string = owned_str });
        try self.string_pool.put(owned_str, idx);
        return idx;
    }

    /// Add identifier constant with deduplication
    fn addIdentifier(self: *Self, name: []const u8) EmitError!u16 {
        if (self.string_pool.get(name)) |idx| {
            return idx;
        }
        // Duplicate the string so the module owns it
        const owned_name = self.allocator.dupe(u8, name) catch return EmitError.OutOfMemory;
        const idx = try self.addConstant(.{ .identifier = owned_name });
        try self.string_pool.put(owned_name, idx);
        return idx;
    }

    /// Track stack push
    fn pushStack(self: *Self, count: u16) void {
        self.current_stack += count;
        if (self.current_stack > self.max_stack) {
            self.max_stack = self.current_stack;
        }
    }

    /// Track stack pop
    fn popStack(self: *Self, count: u16) void {
        if (self.current_stack >= count) {
            self.current_stack -= count;
        }
    }
};

/// Convert IR type to bytecode data type code
fn irTypeToDataType(ty: ir.Type) module.DataTypeCode {
    return switch (ty) {
        .void => .alpha, // Void maps to empty alpha
        .alpha => .alpha,
        .decimal => .decimal,
        .integer => |size| switch (size) {
            1 => .integer1,
            2 => .integer2,
            4 => .integer4,
            8 => .integer8,
            else => .integer4,
        },
        .packed_decimal => .implied_decimal, // Packed decimals use implied decimal encoding
        .ptr => .integer8, // Pointers are 64-bit
        .array => .alpha, // Arrays treated as alpha for now
        .record => .structure, // Records are structures
        .function => .integer8, // Function pointers
    };
}

// ============================================================================
// Public API
// ============================================================================

/// Emit bytecode from IR module
pub fn emitBytecode(allocator: Allocator, ir_module: *const ir.Module) EmitError!module.Module {
    var emitter = BytecodeEmitter.init(allocator);
    defer emitter.deinit();
    return emitter.emit(ir_module);
}

// ============================================================================
// Tests
// ============================================================================

test "emit simple bytecode" {
    const allocator = std.testing.allocator;

    var ir_mod = ir.Module.init(allocator, "test");
    defer ir_mod.deinit();

    const result = try emitBytecode(allocator, &ir_mod);
    defer {
        allocator.free(result.code);
        allocator.free(result.constants);
        allocator.free(result.types);
        allocator.free(result.routines);
    }

    try std.testing.expect(result.header.isValid());
}
