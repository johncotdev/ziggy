//! Ziggy DBL Bytecode Disassembler
//!
//! Provides human-readable output of bytecode for debugging.

const std = @import("std");
const opcodes = @import("opcodes.zig");
const module = @import("module.zig");

const Opcode = opcodes.Opcode;

/// Disassembler for bytecode modules
pub const Disassembler = struct {
    mod: *const module.Module,
    code: []const u8,
    offset: usize,
    writer: std.ArrayList(u8).Writer,

    const Self = @This();

    pub fn init(mod: *const module.Module, writer: std.ArrayList(u8).Writer) Self {
        return .{
            .mod = mod,
            .code = mod.code,
            .offset = 0,
            .writer = writer,
        };
    }

    /// Disassemble entire module
    pub fn disassembleModule(self: *Self) !void {
        try self.writer.print("; Ziggy DBL Bytecode Disassembly\n", .{});
        try self.writer.print("; Version: {}.{}\n", .{
            self.mod.header.version_major,
            self.mod.header.version_minor,
        });
        try self.writer.print("; Entry Point: 0x{X:0>4}\n\n", .{self.mod.header.entry_point});

        // Constants section
        if (self.mod.constants.len > 0) {
            try self.writer.print("; === Constants ({}) ===\n", .{self.mod.constants.len});
            for (self.mod.constants, 0..) |c, i| {
                try self.writer.print(";   [{:0>4}] ", .{i});
                try self.printConstant(c);
                try self.writer.print("\n", .{});
            }
            try self.writer.print("\n", .{});
        }

        // Types section
        if (self.mod.types.len > 0) {
            try self.writer.print("; === Types ({}) ===\n", .{self.mod.types.len});
            for (self.mod.types) |t| {
                try self.printTypeDef(t);
            }
            try self.writer.print("\n", .{});
        }

        // Routines section
        if (self.mod.routines.len > 0) {
            try self.writer.print("; === Routines ({}) ===\n", .{self.mod.routines.len});
            for (self.mod.routines, 0..) |r, i| {
                try self.printRoutineHeader(r, @intCast(i));
            }
            try self.writer.print("\n", .{});
        }

        // Code section
        try self.writer.print("; === Code ({} bytes) ===\n", .{self.code.len});
        try self.disassembleCode(0, self.code.len);
    }

    /// Disassemble a routine
    pub fn disassembleRoutine(self: *Self, routine_idx: u16) !void {
        if (routine_idx >= self.mod.routines.len) return;
        const routine = self.mod.routines[routine_idx];

        try self.printRoutineHeader(routine, routine_idx);
        try self.disassembleCode(routine.code_offset, routine.code_length);
    }

    /// Disassemble code range
    fn disassembleCode(self: *Self, start: usize, length: usize) !void {
        self.offset = start;
        const end = start + length;

        while (self.offset < end) {
            try self.disassembleInstruction();
        }
    }

    /// Disassemble single instruction
    pub fn disassembleInstruction(self: *Self) !void {
        const addr = self.offset;
        const op_byte = self.code[self.offset];
        self.offset += 1;

        const op = std.meta.intToEnum(Opcode, op_byte) catch {
            try self.writer.print("  {X:0>4}:  {X:0>2}              ; <unknown>\n", .{ addr, op_byte });
            return;
        };

        // Print address and opcode
        try self.writer.print("  {X:0>4}:  {X:0>2}", .{ addr, op_byte });

        // Read and print operands
        const operand_size = op.operandSize();
        var operand_bytes: [8]u8 = undefined;
        for (0..operand_size) |i| {
            if (self.offset + i < self.code.len) {
                operand_bytes[i] = self.code[self.offset + i];
                try self.writer.print(" {X:0>2}", .{operand_bytes[i]});
            }
        }
        self.offset += operand_size;

        // Pad to align mnemonic
        const total_bytes = 1 + operand_size;
        const padding = 14 - (total_bytes * 3);
        for (0..padding) |_| {
            try self.writer.print(" ", .{});
        }

        // Print mnemonic
        try self.writer.print(" {s}", .{op.name()});

        // Print operand interpretation
        try self.printOperandInfo(op, operand_bytes[0..operand_size]);

        try self.writer.print("\n", .{});
    }

    fn printOperandInfo(self: *Self, op: Opcode, operands: []const u8) !void {
        switch (op) {
            .push_i8 => {
                const val: i8 = @bitCast(operands[0]);
                try self.writer.print(" {}", .{val});
            },
            .push_i16 => {
                const val: i16 = @bitCast(operands[0..2].*);
                try self.writer.print(" {}", .{val});
            },
            .push_i32 => {
                const val: i32 = @bitCast(operands[0..4].*);
                try self.writer.print(" {}", .{val});
            },
            .push_i64 => {
                const val: i64 = @bitCast(operands[0..8].*);
                try self.writer.print(" {}", .{val});
            },
            .push_const, .push_const_wide => {
                const idx: u16 = if (op == .push_const)
                    @bitCast(operands[0..2].*)
                else
                    @truncate(@as(u32, @bitCast(operands[0..4].*)));
                try self.writer.print(" #{}", .{idx});
                if (idx < self.mod.constants.len) {
                    try self.writer.print("  ; ", .{});
                    try self.printConstant(self.mod.constants[idx]);
                }
            },
            .load_local, .store_local => {
                try self.writer.print(" ${}", .{operands[0]});
            },
            .load_local_wide, .store_local_wide => {
                const slot: u16 = @bitCast(operands[0..2].*);
                try self.writer.print(" ${}", .{slot});
            },
            .load_global, .store_global => {
                const idx: u16 = @bitCast(operands[0..2].*);
                try self.writer.print(" @{}", .{idx});
            },
            .call, .call_external, .call_native => {
                const idx: u16 = @bitCast(operands[0..2].*);
                try self.writer.print(" routine#{}", .{idx});
            },
            .jump, .jump_if_true, .jump_if_false, .jump_if_null, .jump_eq, .jump_ne, .jump_lt, .jump_ge => {
                const offset_val: i16 = @bitCast(operands[0..2].*);
                const addr_after: i32 = @intCast(self.offset);
                const target = addr_after + offset_val;
                try self.writer.print(" -> {X:0>4}", .{@as(u32, @intCast(target))});
            },
            .jump_wide => {
                const offset_val: i32 = @bitCast(operands[0..4].*);
                const addr_after: i32 = @intCast(self.offset);
                const target = addr_after + offset_val;
                try self.writer.print(" -> {X:0>4}", .{@as(u32, @intCast(target))});
            },
            .ch_display, .ch_accept => {
                try self.writer.print(" argc={}", .{operands[0]});
            },
            .ch_open => {
                const flags: opcodes.OpenModeFlags = @bitCast(operands[0]);
                try self.writer.print(" ", .{});
                if (flags.input) try self.writer.print("I", .{});
                if (flags.output) try self.writer.print("O", .{});
                if (flags.update) try self.writer.print("U", .{});
                if (flags.append) try self.writer.print("A", .{});
                if (flags.isam) try self.writer.print(":I", .{});
            },
            .isam_read => {
                try self.writer.print(" key={}, mode={s}", .{
                    operands[0],
                    @tagName(std.meta.intToEnum(opcodes.MatchMode, operands[1]) catch .exact),
                });
            },
            .xcall => {
                const name_idx: u16 = @bitCast(operands[0..2].*);
                const argc = operands[2];
                try self.writer.print(" #{} argc={}", .{ name_idx, argc });
                if (name_idx < self.mod.constants.len) {
                    try self.writer.print("  ; ", .{});
                    try self.printConstant(self.mod.constants[name_idx]);
                }
            },
            .new_record, .load_field, .store_field => {
                const idx: u16 = @bitCast(operands[0..2].*);
                try self.writer.print(" type#{}", .{idx});
            },
            .debug_line => {
                const line: u16 = @bitCast(operands[0..2].*);
                try self.writer.print(" line {}", .{line});
            },
            .to_dec, .fn_round => {
                try self.writer.print(" prec={}", .{operands[0]});
            },
            .to_alpha => {
                const size: u16 = @bitCast(operands[0..2].*);
                try self.writer.print(" size={}", .{size});
            },
            else => {},
        }
    }

    fn printConstant(self: *Self, c: module.Constant) !void {
        switch (c) {
            .integer => |v| try self.writer.print("int({})", .{v}),
            .decimal => |d| try self.writer.print("dec({}, prec={})", .{ d.value, d.precision }),
            .string => |s| try self.writer.print("str(\"{}\")", .{std.zig.fmtEscapes(s)}),
            .alpha => |a| try self.writer.print("alpha({}, size={})", .{ std.zig.fmtEscapes(a.data), a.size }),
            .identifier => |s| try self.writer.print("id({})", .{std.zig.fmtEscapes(s)}),
            .record_ref => |r| try self.writer.print("record#{}", .{r}),
            .routine_ref => |r| try self.writer.print("routine#{}", .{r}),
        }
    }

    fn printTypeDef(self: *Self, t: module.TypeDef) !void {
        try self.writer.print(";   type#{}: ", .{t.type_id});
        if (t.name_index < self.mod.constants.len) {
            switch (self.mod.constants[t.name_index]) {
                .identifier => |name| try self.writer.print("{s}", .{name}),
                else => try self.writer.print("<unknown>", .{}),
            }
        }
        try self.writer.print(" (size={}, fields={})\n", .{ t.total_size, t.fields.len });
        for (t.fields) |f| {
            try self.writer.print(";       +{}: ", .{f.offset});
            if (f.name_index < self.mod.constants.len) {
                switch (self.mod.constants[f.name_index]) {
                    .identifier => |name| try self.writer.print("{s}", .{name}),
                    else => try self.writer.print("<?>", .{}),
                }
            }
            try self.writer.print(" : {s}({})\n", .{ @tagName(f.data_type), f.size });
        }
    }

    fn printRoutineHeader(self: *Self, r: module.RoutineDef, idx: u16) !void {
        try self.writer.print(";   routine#{}: ", .{idx});
        if (r.name_index < self.mod.constants.len) {
            switch (self.mod.constants[r.name_index]) {
                .identifier => |name| try self.writer.print("{s}", .{name}),
                else => try self.writer.print("<unknown>", .{}),
            }
        }
        try self.writer.print(" @ 0x{X:0>4} ({} bytes)", .{ r.code_offset, r.code_length });
        if (r.flags.is_function) try self.writer.print(" [FUNC]", .{});
        if (r.flags.is_subroutine) try self.writer.print(" [SUB]", .{});
        try self.writer.print(" locals={} stack={}\n", .{ r.local_count, r.max_stack });
    }
};

/// Disassemble a module to a string
pub fn disassemble(allocator: std.mem.Allocator, mod: *const module.Module) ![]u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    errdefer buffer.deinit();

    var disasm = Disassembler.init(mod, buffer.writer());
    try disasm.disassembleModule();

    return buffer.toOwnedSlice();
}

/// Disassemble code bytes directly (without module context)
pub fn disassembleBytes(allocator: std.mem.Allocator, code: []const u8) ![]u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    errdefer buffer.deinit();

    // Create a minimal module wrapper
    var mod = module.Module.init(allocator);
    mod.code = code;

    var disasm = Disassembler.init(&mod, buffer.writer());
    try disasm.disassembleCode(0, code.len);

    return buffer.toOwnedSlice();
}

test "disassemble simple code" {
    const allocator = std.testing.allocator;

    // Simple: push_i8 42, push_i8 10, add, halt
    const code = [_]u8{
        0x04, 42, // push_i8 42
        0x04, 10, // push_i8 10
        0x40, // add
        0xFF, // halt
    };

    const result = try disassembleBytes(allocator, &code);
    defer allocator.free(result);

    try std.testing.expect(std.mem.indexOf(u8, result, "push_i8 42") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "push_i8 10") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "add") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "halt") != null);
}
