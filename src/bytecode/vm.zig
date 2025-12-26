//! Ziggy DBL Virtual Machine
//!
//! Stack-based bytecode interpreter for compiled Ziggy DBL programs.

const std = @import("std");
const Opcode = @import("opcodes.zig").Opcode;
const Module = @import("module.zig").Module;
const Constant = @import("module.zig").Constant;

/// Maximum stack size
const STACK_SIZE = 4096;

/// Maximum call depth
const MAX_CALL_DEPTH = 256;

/// Maximum channels
const MAX_CHANNELS = 1024;

/// VM error types
pub const VMError = error{
    StackOverflow,
    StackUnderflow,
    InvalidOpcode,
    InvalidConstant,
    InvalidType,
    InvalidRoutine,
    DivisionByZero,
    OutOfMemory,
    FileError,
    IsamError,
    UnresolvedImport,
    CallStackOverflow,
    Halted,
};

/// Runtime value
pub const Value = union(enum) {
    null_val: void,
    boolean: bool,
    integer: i64,
    decimal: struct { value: i64, precision: u8 },
    alpha: []u8,
    string: []const u8,
    record_ref: *Record,
    handle: usize,

    pub fn toInt(self: Value) i64 {
        return switch (self) {
            .integer => |i| i,
            .decimal => |d| @divTrunc(d.value, std.math.pow(i64, 10, d.precision)),
            .boolean => |b| if (b) @as(i64, 1) else 0,
            .alpha => |a| std.fmt.parseInt(i64, std.mem.trim(u8, a, " "), 10) catch 0,
            .string => |s| std.fmt.parseInt(i64, s, 10) catch 0,
            else => 0,
        };
    }

    pub fn toBool(self: Value) bool {
        return switch (self) {
            .null_val => false,
            .boolean => |b| b,
            .integer => |i| i != 0,
            .decimal => |d| d.value != 0,
            .alpha => |a| a.len > 0 and !std.mem.allEqual(u8, a, ' '),
            .string => |s| s.len > 0,
            else => true,
        };
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .null_val => try writer.writeAll("null"),
            .boolean => |b| try writer.print("{}", .{b}),
            .integer => |i| try writer.print("{d}", .{i}),
            .decimal => |d| {
                const divisor = std.math.pow(i64, 10, d.precision);
                const whole = @divTrunc(d.value, divisor);
                const frac = @abs(@rem(d.value, divisor));
                try writer.print("{d}.{d}", .{ whole, frac });
            },
            .alpha => |a| try writer.print("{s}", .{a}),
            .string => |s| try writer.print("{s}", .{s}),
            .record_ref => try writer.writeAll("<record>"),
            .handle => |h| try writer.print("<handle:{d}>", .{h}),
        }
    }
};

/// Runtime record instance
pub const Record = struct {
    type_id: u16,
    data: []u8,
};

/// Call frame
pub const CallFrame = struct {
    module: *const Module,
    routine_index: u16,
    return_ip: usize,
    base_pointer: usize,
};

/// Simple bounded call stack (replacement for removed BoundedArray)
pub const CallStack = struct {
    buffer: [MAX_CALL_DEPTH]CallFrame = undefined,
    len: usize = 0,

    pub fn append(self: *CallStack, item: CallFrame) !void {
        if (self.len >= MAX_CALL_DEPTH) return error.CallStackOverflow;
        self.buffer[self.len] = item;
        self.len += 1;
    }

    pub fn pop(self: *CallStack) ?CallFrame {
        if (self.len == 0) return null;
        self.len -= 1;
        return self.buffer[self.len];
    }

    pub fn slice(self: *CallStack) []CallFrame {
        return self.buffer[0..self.len];
    }
};

/// Channel state
pub const Channel = struct {
    file: ?std.fs.File,
    isam: ?*anyopaque, // IsamFile pointer
    is_terminal: bool,
    is_open: bool,
};

/// Virtual Machine
pub const VM = struct {
    allocator: std.mem.Allocator,

    // Execution state
    ip: usize,
    sp: usize,
    fp: usize,

    // Memory
    stack: [STACK_SIZE]Value,
    globals: std.ArrayList(Value),
    heap: std.heap.ArenaAllocator,

    // Call stack
    call_stack: CallStack,

    // I/O
    channels: [MAX_CHANNELS]Channel,
    stdout: std.fs.File,

    // Modules
    current_module: ?*const Module,
    modules: std.ArrayList(*const Module),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        var channels: [MAX_CHANNELS]Channel = undefined;
        for (&channels) |*ch| {
            ch.* = .{
                .file = null,
                .isam = null,
                .is_terminal = false,
                .is_open = false,
            };
        }

        return .{
            .allocator = allocator,
            .ip = 0,
            .sp = 0,
            .fp = 0,
            .stack = undefined,
            .globals = .{},
            .heap = std.heap.ArenaAllocator.init(allocator),
            .call_stack = .{},
            .channels = channels,
            .stdout = std.fs.File.stdout(),
            .current_module = null,
            .modules = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        // Close channels
        for (&self.channels) |*ch| {
            if (ch.file) |*f| f.close();
        }

        self.globals.deinit(self.allocator);
        self.heap.deinit();
        self.modules.deinit(self.allocator);
    }

    /// Load a module
    pub fn loadModule(self: *Self, module: *const Module) !void {
        try self.modules.append(self.allocator, module);
    }

    /// Execute a module
    pub fn execute(self: *Self, module: *const Module) VMError!void {
        self.current_module = module;

        // Find entry point
        if (module.header.entry_point == 0xFFFFFFFF) {
            return VMError.InvalidRoutine; // Library, no entry point
        }

        self.ip = module.header.entry_point;

        try self.run();
    }

    /// Main execution loop
    fn run(self: *Self) VMError!void {
        const module = self.current_module orelse return VMError.InvalidRoutine;

        while (self.ip < module.code.len) {
            const opcode: Opcode = @enumFromInt(module.code[self.ip]);
            self.ip += 1;

            switch (opcode) {
                // Stack operations
                .nop => {},

                .push_null => try self.push(.{ .null_val = {} }),
                .push_true => try self.push(.{ .boolean = true }),
                .push_false => try self.push(.{ .boolean = false }),

                .push_i8 => {
                    const val: i8 = @bitCast(module.code[self.ip]);
                    self.ip += 1;
                    try self.push(.{ .integer = val });
                },

                .push_i16 => {
                    const val = self.readI16(module);
                    try self.push(.{ .integer = val });
                },

                .push_i32 => {
                    const val = self.readI32(module);
                    try self.push(.{ .integer = val });
                },

                .push_i64 => {
                    const val = self.readI64(module);
                    try self.push(.{ .integer = val });
                },

                .push_const => {
                    const idx = self.readU16(module);
                    const constant = module.getConstant(idx) orelse
                        return VMError.InvalidConstant;
                    try self.pushConstant(constant);
                },

                .pop => _ = try self.pop(),
                .dup => {
                    const val = try self.peek();
                    try self.push(val);
                },
                .swap => {
                    const a = try self.pop();
                    const b = try self.pop();
                    try self.push(a);
                    try self.push(b);
                },

                // Local variables
                .load_local_0 => try self.push(self.stack[self.fp]),
                .load_local_1 => try self.push(self.stack[self.fp + 1]),
                .load_local_2 => try self.push(self.stack[self.fp + 2]),
                .load_local_3 => try self.push(self.stack[self.fp + 3]),

                .load_local => {
                    const slot = module.code[self.ip];
                    self.ip += 1;
                    try self.push(self.stack[self.fp + slot]);
                },

                .store_local_0 => self.stack[self.fp] = try self.pop(),
                .store_local_1 => self.stack[self.fp + 1] = try self.pop(),
                .store_local_2 => self.stack[self.fp + 2] = try self.pop(),
                .store_local_3 => self.stack[self.fp + 3] = try self.pop(),

                .store_local => {
                    const slot = module.code[self.ip];
                    self.ip += 1;
                    self.stack[self.fp + slot] = try self.pop();
                },

                // Global variables
                .load_global => {
                    const idx = self.readU16(module);
                    if (idx >= self.globals.items.len) {
                        try self.push(.{ .null_val = {} });
                    } else {
                        try self.push(self.globals.items[idx]);
                    }
                },

                .store_global => {
                    const idx = self.readU16(module);
                    const val = try self.pop();
                    while (self.globals.items.len <= idx) {
                        self.globals.append(self.allocator, .{ .null_val = {} }) catch
                            return VMError.OutOfMemory;
                    }
                    self.globals.items[idx] = val;
                },

                // Arithmetic
                .add => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .integer = a.toInt() + b.toInt() });
                },

                .sub => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .integer = a.toInt() - b.toInt() });
                },

                .mul => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .integer = a.toInt() * b.toInt() });
                },

                .div => {
                    const b = try self.pop();
                    const a = try self.pop();
                    if (b.toInt() == 0) return VMError.DivisionByZero;
                    try self.push(.{ .integer = @divTrunc(a.toInt(), b.toInt()) });
                },

                .mod => {
                    const b = try self.pop();
                    const a = try self.pop();
                    if (b.toInt() == 0) return VMError.DivisionByZero;
                    try self.push(.{ .integer = @rem(a.toInt(), b.toInt()) });
                },

                .neg => {
                    const a = try self.pop();
                    try self.push(.{ .integer = -a.toInt() });
                },

                .incr => {
                    const a = try self.pop();
                    try self.push(.{ .integer = a.toInt() + 1 });
                },

                .decr => {
                    const a = try self.pop();
                    try self.push(.{ .integer = a.toInt() - 1 });
                },

                // Comparison
                .cmp_eq => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .boolean = a.toInt() == b.toInt() });
                },

                .cmp_ne => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .boolean = a.toInt() != b.toInt() });
                },

                .cmp_lt => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .boolean = a.toInt() < b.toInt() });
                },

                .cmp_le => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .boolean = a.toInt() <= b.toInt() });
                },

                .cmp_gt => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .boolean = a.toInt() > b.toInt() });
                },

                .cmp_ge => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .boolean = a.toInt() >= b.toInt() });
                },

                // Logical
                .log_and => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .boolean = a.toBool() and b.toBool() });
                },

                .log_or => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .boolean = a.toBool() or b.toBool() });
                },

                .log_not => {
                    const a = try self.pop();
                    try self.push(.{ .boolean = !a.toBool() });
                },

                // Control flow
                .jump => {
                    const offset = self.readI16(module);
                    self.ip = @intCast(@as(i64, @intCast(self.ip)) + offset);
                },

                .jump_if_true => {
                    const offset = self.readI16(module);
                    const cond = try self.pop();
                    if (cond.toBool()) {
                        self.ip = @intCast(@as(i64, @intCast(self.ip)) + offset);
                    }
                },

                .jump_if_false => {
                    const offset = self.readI16(module);
                    const cond = try self.pop();
                    if (!cond.toBool()) {
                        self.ip = @intCast(@as(i64, @intCast(self.ip)) + offset);
                    }
                },

                // Subroutine calls
                .call => {
                    const routine_idx = self.readU16(module);
                    try self.callRoutine(routine_idx);
                },

                .ret => {
                    if (self.call_stack.len == 0) {
                        return; // Return from main
                    }
                    const frame = self.call_stack.pop().?;
                    self.ip = frame.return_ip;
                    self.sp = frame.base_pointer;
                    self.current_module = frame.module;
                },

                .ret_val => {
                    const val = try self.pop();
                    if (self.call_stack.len == 0) {
                        return;
                    }
                    const frame = self.call_stack.pop().?;
                    self.ip = frame.return_ip;
                    self.sp = frame.base_pointer;
                    self.current_module = frame.module;
                    try self.push(val);
                },

                // I/O
                .ch_display => {
                    const arg_count = module.code[self.ip];
                    self.ip += 1;
                    try self.executeDisplay(arg_count);
                },

                .ch_open => {
                    const flags = module.code[self.ip];
                    self.ip += 1;
                    try self.executeOpen(flags);
                },

                .ch_close => {
                    try self.executeClose();
                },

                // Halt
                .halt => return,

                else => {
                    // Unimplemented opcode
                    std.debug.print("Unimplemented opcode: {s}\n", .{opcode.name()});
                    return VMError.InvalidOpcode;
                },
            }
        }
    }

    // Stack operations
    fn push(self: *Self, value: Value) VMError!void {
        if (self.sp >= STACK_SIZE) return VMError.StackOverflow;
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    fn pop(self: *Self) VMError!Value {
        if (self.sp == 0) return VMError.StackUnderflow;
        self.sp -= 1;
        return self.stack[self.sp];
    }

    fn peek(self: *Self) VMError!Value {
        if (self.sp == 0) return VMError.StackUnderflow;
        return self.stack[self.sp - 1];
    }

    fn pushConstant(self: *Self, constant: Constant) VMError!void {
        const value: Value = switch (constant) {
            .integer => |ival| .{ .integer = ival },
            .decimal => |dval| .{ .decimal = .{ .value = dval.value, .precision = dval.precision } },
            .string => |sval| .{ .string = sval },
            .alpha => |aval| .{ .alpha = @constCast(aval.data) },
            .identifier => |idval| .{ .string = idval }, // Identifiers are strings
            else => .{ .null_val = {} },
        };
        try self.push(value);
    }

    // Read helpers
    fn readU16(self: *Self, module: *const Module) u16 {
        const val = std.mem.readInt(u16, module.code[self.ip..][0..2], .little);
        self.ip += 2;
        return val;
    }

    fn readI16(self: *Self, module: *const Module) i16 {
        const val = std.mem.readInt(i16, module.code[self.ip..][0..2], .little);
        self.ip += 2;
        return val;
    }

    fn readI32(self: *Self, module: *const Module) i32 {
        const val = std.mem.readInt(i32, module.code[self.ip..][0..4], .little);
        self.ip += 4;
        return val;
    }

    fn readI64(self: *Self, module: *const Module) i64 {
        const val = std.mem.readInt(i64, module.code[self.ip..][0..8], .little);
        self.ip += 8;
        return val;
    }

    // Call routine
    fn callRoutine(self: *Self, routine_idx: u16) VMError!void {
        const module = self.current_module orelse return VMError.InvalidRoutine;
        const routine = module.getRoutine(routine_idx) orelse
            return VMError.InvalidRoutine;

        // Save call frame
        self.call_stack.append(.{
            .module = module,
            .routine_index = routine_idx,
            .return_ip = self.ip,
            .base_pointer = self.fp,
        }) catch return VMError.CallStackOverflow;

        // Set up new frame
        self.fp = self.sp;
        self.ip = routine.code_offset;
    }

    // I/O implementations
    fn executeDisplay(self: *Self, arg_count: u8) VMError!void {
        // Pop arguments in reverse
        var args: [16]Value = undefined;
        var i: usize = arg_count;
        while (i > 0) {
            i -= 1;
            args[i] = try self.pop();
        }

        // Pop channel
        const channel = (try self.pop()).toInt();

        // Get writer
        var write_buffer: [4096]u8 = undefined;
        var buffered = self.stdout.writer(&write_buffer);
        const writer = &buffered.interface;

        // Write arguments - format each value appropriately
        for (args[0..arg_count]) |arg| {
            switch (arg) {
                .null_val => writer.writeAll("") catch {},
                .boolean => |bval| if (bval) {
                    writer.writeAll("1") catch {};
                } else {
                    writer.writeAll("0") catch {};
                },
                .integer => |ival| writer.print("{d}", .{ival}) catch {},
                .decimal => |dval| {
                    const divisor = std.math.pow(i64, 10, dval.precision);
                    const whole = @divTrunc(dval.value, divisor);
                    const frac = @abs(@rem(dval.value, divisor));
                    writer.print("{d}.{d}", .{ whole, frac }) catch {};
                },
                .alpha => |aval| writer.print("{s}", .{aval}) catch {},
                .string => |sval| writer.print("{s}", .{sval}) catch {},
                .record_ref => writer.writeAll("<record>") catch {},
                .handle => |hval| writer.print("<handle:{d}>", .{hval}) catch {},
            }
        }
        writer.writeByte('\n') catch {};
        writer.flush() catch {};
        _ = channel;
    }

    fn executeOpen(self: *Self, flags: u8) VMError!void {
        _ = flags; // Mode is encoded in flags, not on stack
        _ = try self.pop(); // filename
        const channel = (try self.pop()).toInt();

        if (channel >= 0 and channel < MAX_CHANNELS) {
            self.channels[@intCast(channel)].is_open = true;
        }
    }

    fn executeClose(self: *Self) VMError!void {
        const channel = (try self.pop()).toInt();

        if (channel >= 0 and channel < MAX_CHANNELS) {
            var ch = &self.channels[@intCast(channel)];
            if (ch.file) |*f| {
                f.close();
                ch.file = null;
            }
            ch.is_open = false;
        }
    }
};

test "vm basic operations" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // Test stack operations
    try vm.push(.{ .integer = 42 });
    try vm.push(.{ .integer = 10 });

    const b = try vm.pop();
    const a = try vm.pop();

    try std.testing.expectEqual(@as(i64, 42), a.toInt());
    try std.testing.expectEqual(@as(i64, 10), b.toInt());
}
