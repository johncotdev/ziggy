//! Ziggy DBL Virtual Machine
//!
//! Stack-based bytecode interpreter for compiled Ziggy DBL programs.

const std = @import("std");
const Opcode = @import("opcodes.zig").Opcode;
const opcodes = @import("opcodes.zig");
const Module = @import("module.zig").Module;
const Constant = @import("module.zig").Constant;
const isam = @import("../isam/isam.zig");
const subroutines = @import("../subroutines/subroutines.zig");

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
    isam_file: ?*isam.IsamFile,
    is_terminal: bool,
    is_open: bool,
    filename: ?[]const u8,
    key_of_reference: u8, // Current key index for READS (set by READ/FIND)
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
    global_buffers: std.ArrayList([]u8), // For alpha/string globals
    heap: std.heap.ArenaAllocator,

    // Call stack
    call_stack: CallStack,

    // I/O
    channels: [MAX_CHANNELS]Channel,
    stdout: std.fs.File,

    // Subroutines
    subroutine_registry: subroutines.SubroutineRegistry,
    channel_manager: subroutines.ChannelManager,

    // Modules
    current_module: ?*const Module,
    modules: std.ArrayList(*const Module),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        var channels: [MAX_CHANNELS]Channel = undefined;
        for (&channels) |*ch| {
            ch.* = .{
                .file = null,
                .isam_file = null,
                .is_terminal = false,
                .is_open = false,
                .filename = null,
                .key_of_reference = 0, // Default to primary key
            };
        }

        return .{
            .allocator = allocator,
            .ip = 0,
            .sp = 0,
            .fp = 0,
            .stack = undefined,
            .globals = .{},
            .global_buffers = .{},
            .heap = std.heap.ArenaAllocator.init(allocator),
            .call_stack = .{},
            .channels = channels,
            .stdout = std.fs.File.stdout(),
            .subroutine_registry = subroutines.SubroutineRegistry.init(allocator),
            .channel_manager = subroutines.ChannelManager.init(allocator),
            .current_module = null,
            .modules = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        // Close channels
        for (&self.channels) |*ch| {
            if (ch.file) |*f| f.close();
            if (ch.isam_file) |isam_f| isam_f.close();
        }

        // Free global buffers
        for (self.global_buffers.items) |buf| {
            self.allocator.free(buf);
        }
        self.global_buffers.deinit(self.allocator);

        self.globals.deinit(self.allocator);
        self.heap.deinit();
        self.modules.deinit(self.allocator);
        self.subroutine_registry.deinit();
        self.channel_manager.deinit();
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

                // Load record buffer (build from field globals)
                .load_record_buf => {
                    const type_idx = self.readU16(module);
                    try self.executeLoadRecordBuf(module, type_idx);
                },

                // Store record buffer (distribute to field globals)
                .store_record_buf => {
                    const type_idx = self.readU16(module);
                    try self.executeStoreRecordBuf(module, type_idx);
                },

                // XCALL - External subroutine call
                .xcall => {
                    const name_idx = self.readU16(module);
                    const arg_count = module.code[self.ip];
                    self.ip += 1;
                    try self.executeXcall(module, name_idx, arg_count);
                },

                // ISAM Store
                .isam_store => {
                    try self.executeIsamStore();
                },

                // Keyed read (READ with key) - sets key of reference
                .isam_read => {
                    const key_num = module.code[self.ip];
                    self.ip += 1;
                    const match_mode = module.code[self.ip];
                    self.ip += 1;
                    try self.executeIsamRead(key_num, match_mode);
                },

                // Sequential read (ch_read has no key param, ch_reads has key param)
                // SynergyDE: READS uses channel's key_of_reference (set by READ/FIND)
                .ch_read => {
                    // Use channel's key of reference
                    try self.executeReads(255); // 255 = use channel's key_of_reference
                },
                .ch_reads => {
                    // Backward compat: if KEYNUM specified, use it; otherwise use channel's
                    const key_num = module.code[self.ip];
                    self.ip += 1;
                    try self.executeReads(key_num);
                },
                .isam_reads => {
                    // Use channel's key of reference
                    try self.executeReads(255);
                },

                // String concatenation
                .str_concat => {
                    const b = try self.pop();
                    const a = try self.pop();
                    const result = try self.concatValues(a, b);
                    try self.push(result);
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
        const mode_flags: opcodes.OpenModeFlags = @bitCast(flags);
        const filename_val = try self.pop();
        const channel_val = try self.pop();
        const channel_num = channel_val.toInt();

        if (channel_num < 0 or channel_num >= MAX_CHANNELS) {
            return VMError.FileError;
        }

        const ch = &self.channels[@intCast(channel_num)];

        // Get filename string
        const filename = switch (filename_val) {
            .string => |s| s,
            .alpha => |a| std.mem.trim(u8, a, " "),
            else => return VMError.InvalidType,
        };

        // Check for terminal
        if (std.mem.eql(u8, filename, "tt:") or std.mem.eql(u8, filename, "TT:")) {
            ch.is_terminal = true;
            ch.is_open = true;
            return;
        }

        // ISAM file open
        if (mode_flags.isam) {
            const isam_file = isam.IsamFile.open(self.allocator, filename, .read_write) catch {
                return VMError.FileError;
            };
            ch.isam_file = isam_file;
            ch.is_open = true;
            ch.filename = filename;
        } else {
            // Regular file open
            const file = std.fs.cwd().openFile(filename, .{ .mode = .read_write }) catch {
                return VMError.FileError;
            };
            ch.file = file;
            ch.is_open = true;
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
            if (ch.isam_file) |isam_f| {
                isam_f.close();
                ch.isam_file = null;
            }
            ch.is_open = false;
        }
    }

    fn executeLoadRecordBuf(self: *Self, module: *const Module, type_idx: u16) VMError!void {
        // Get the type definition
        if (type_idx >= module.types.len) {
            return VMError.InvalidType;
        }

        const type_def = &module.types[type_idx];

        // Allocate buffer for record
        const buf = self.allocator.alloc(u8, type_def.total_size) catch return VMError.OutOfMemory;
        @memset(buf, ' '); // Initialize with spaces
        try self.global_buffers.append(self.allocator, buf);

        // Calculate the global base offset for this record type
        // Globals are created in order of record definition, then field order within each record
        var global_base: u16 = 0;
        for (module.types[0..type_idx]) |t| {
            global_base += @intCast(t.fields.len);
        }

        // Fill buffer from field globals
        for (type_def.fields, 0..) |field, field_idx| {
            const global_slot = global_base + @as(u16, @intCast(field_idx));

            if (global_slot < self.globals.items.len) {
                const val = self.globals.items[global_slot];
                const offset = field.offset;
                const end = @min(offset + field.size, type_def.total_size);

                // Copy value into buffer at field offset
                switch (val) {
                    .alpha => |a| {
                        const copy_len = @min(a.len, end - offset);
                        @memcpy(buf[offset .. offset + copy_len], a[0..copy_len]);
                    },
                    .string => |s| {
                        const copy_len = @min(s.len, end - offset);
                        @memcpy(buf[offset .. offset + copy_len], s[0..copy_len]);
                    },
                    .integer => |i| {
                        // Format integer into buffer
                        var tmp: [32]u8 = undefined;
                        const str = std.fmt.bufPrint(&tmp, "{d}", .{i}) catch continue;
                        // Right-align in field, pad with spaces on left for numeric
                        if (str.len <= field.size) {
                            const start = offset + field.size - str.len;
                            @memcpy(buf[start .. start + str.len], str);
                        }
                    },
                    else => {},
                }
            }
        }

        try self.push(.{ .alpha = buf });
    }

    fn executeStoreRecordBuf(self: *Self, module: *const Module, type_idx: u16) VMError!void {
        // Pop the record buffer from stack
        const record_val = try self.pop();
        const buf = switch (record_val) {
            .alpha => |a| a,
            .string => |s| s,
            else => return VMError.InvalidType,
        };

        // Get the type definition
        if (type_idx >= module.types.len) {
            return VMError.InvalidType;
        }

        const type_def = &module.types[type_idx];

        // Calculate the global base offset for this record type
        var global_base: u16 = 0;
        for (module.types[0..type_idx]) |t| {
            global_base += @intCast(t.fields.len);
        }

        // Distribute buffer data to field globals
        for (type_def.fields, 0..) |field, field_idx| {
            const global_slot = global_base + @as(u16, @intCast(field_idx));
            const offset = field.offset;
            const end = @min(offset + field.size, buf.len);

            if (offset < buf.len) {
                // Extract field data from buffer
                const field_data = buf[offset..@min(end, buf.len)];

                // Allocate copy for the field value
                const field_copy = self.allocator.alloc(u8, field_data.len) catch return VMError.OutOfMemory;
                @memcpy(field_copy, field_data);
                try self.global_buffers.append(self.allocator, field_copy);

                // Ensure globals array is big enough
                while (self.globals.items.len <= global_slot) {
                    try self.globals.append(self.allocator, .{ .null_val = {} });
                }

                // Store the field value
                self.globals.items[global_slot] = .{ .alpha = field_copy };
            }
        }
    }

    fn executeXcall(self: *Self, module: *const Module, name_idx: u16, arg_count: u8) VMError!void {
        // Get routine name from constants
        const name_const = module.getConstant(name_idx) orelse return VMError.InvalidConstant;
        const routine_name = switch (name_const) {
            .identifier => |n| n,
            .string => |s| s,
            else => return VMError.InvalidConstant,
        };

        // Pop arguments in reverse order
        var args: [16]subroutines.Value = undefined;
        var i: usize = arg_count;
        while (i > 0) {
            i -= 1;
            const val = try self.pop();
            args[i] = self.valueToSubroutineValue(val);
        }

        // Create subroutine context
        var ctx = subroutines.SubroutineContext{
            .allocator = self.allocator,
            .args = args[0..arg_count],
            .channels = &self.channel_manager,
        };

        // Call the subroutine
        const result = self.subroutine_registry.call(routine_name, &ctx) catch |err| {
            std.debug.print("XCALL {s} failed: {}\n", .{ routine_name, err });
            return VMError.InvalidOpcode;
        };

        // Push result if any
        if (result) |res| {
            try self.push(self.subroutineValueToValue(res));
        }
    }

    fn valueToSubroutineValue(self: *Self, val: Value) subroutines.Value {
        _ = self;
        return switch (val) {
            .null_val => .{ .null_val = {} },
            .boolean => |b| .{ .integer = if (b) 1 else 0 },
            .integer => |i| .{ .integer = i },
            .decimal => |d| .{ .decimal = d.value }, // Runtime uses plain i64 for decimal
            .alpha => |a| .{ .alpha = a },
            .string => |s| .{ .string = s },
            else => .{ .null_val = {} },
        };
    }

    fn subroutineValueToValue(self: *Self, val: subroutines.Value) Value {
        _ = self;
        return switch (val) {
            .null_val => .{ .null_val = {} },
            .integer => |i| .{ .integer = i },
            .decimal => |d| .{ .decimal = .{ .value = d, .precision = 0 } }, // Convert back
            .alpha => |a| .{ .alpha = a },
            .string => |s| .{ .string = s },
            else => .{ .null_val = {} },
        };
    }

    fn executeIsamStore(self: *Self) VMError!void {
        const record_val = try self.pop();
        const channel_val = try self.pop();
        const channel_num = channel_val.toInt();

        if (channel_num < 0 or channel_num >= MAX_CHANNELS) {
            return VMError.FileError;
        }

        const ch = &self.channels[@intCast(channel_num)];
        if (!ch.is_open) {
            return VMError.FileError;
        }

        // Get record data
        const record_data = switch (record_val) {
            .alpha => |a| a,
            .string => |s| @constCast(s),
            else => return VMError.InvalidType,
        };

        // Store to ISAM file
        if (ch.isam_file) |isam_f| {
            _ = isam_f.store(record_data) catch return VMError.IsamError;
        } else {
            return VMError.FileError;
        }
    }

    fn executeReads(self: *Self, key_num_param: u8) VMError!void {
        const channel_val = try self.pop();
        const channel_num = channel_val.toInt();

        if (channel_num < 0 or channel_num >= MAX_CHANNELS) {
            return VMError.FileError;
        }

        const ch = &self.channels[@intCast(channel_num)];
        if (!ch.is_open) {
            return VMError.FileError;
        }

        // Determine key number to use:
        // - 255 means "use channel's key_of_reference" (SynergyDE behavior)
        // - 0 could mean either "primary key" or "use channel's key_of_reference"
        //   For backward compat, 0 means primary key unless it was set by a prior READ
        // - Any other value is explicit KEYNUM (backward compat, non-standard)
        var key_num: u8 = undefined;
        if (key_num_param == 255) {
            // Use channel's key of reference (SynergyDE behavior)
            key_num = ch.key_of_reference;
        } else {
            // Explicit key number (backward compat for KEYNUM on READS)
            key_num = key_num_param;
            // Update channel's key of reference for consistency
            ch.key_of_reference = key_num;
        }

        // Read next record from ISAM file using determined key
        if (ch.isam_file) |isam_f| {
            var record_buf: [4096]u8 = undefined;
            const len = isam_f.readNextByKey(key_num, &record_buf) catch |err| {
                if (err == isam.IsamError.EndOfFile) {
                    return VMError.FileError; // EOF
                }
                return VMError.IsamError;
            };

            // Allocate buffer for the record
            const result = self.allocator.alloc(u8, len) catch return VMError.OutOfMemory;
            @memcpy(result, record_buf[0..len]);
            try self.global_buffers.append(self.allocator, result);

            try self.push(.{ .alpha = result });
        } else {
            return VMError.FileError;
        }
    }

    fn executeIsamRead(self: *Self, key_num: u8, match_mode_byte: u8) VMError!void {
        // Pop key value and channel
        const key_val = try self.pop();
        const channel_val = try self.pop();
        const channel_num = channel_val.toInt();

        if (channel_num < 0 or channel_num >= MAX_CHANNELS) {
            return VMError.FileError;
        }

        const ch = &self.channels[@intCast(channel_num)];
        if (!ch.is_open) {
            return VMError.FileError;
        }

        // Set channel's key of reference (for subsequent READS)
        ch.key_of_reference = key_num;

        // Get key string
        const key_str = switch (key_val) {
            .alpha => |s| s,
            .string => |s| @constCast(s),
            .integer => |i| blk: {
                const buf = self.allocator.alloc(u8, 32) catch return VMError.OutOfMemory;
                const formatted = std.fmt.bufPrint(buf, "{d}", .{i}) catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, buf);
                break :blk buf[0..formatted.len];
            },
            else => return VMError.InvalidType,
        };

        // Perform keyed read
        if (ch.isam_file) |isam_f| {
            var record_buf: [4096]u8 = undefined;

            const match_mode: isam.MatchMode = @enumFromInt(match_mode_byte);
            const len = isam_f.read(key_str, &record_buf, .{
                .key_number = key_num,
                .match_mode = match_mode,
            }) catch |err| {
                return switch (err) {
                    isam.IsamError.KeyNotFound => VMError.FileError,
                    isam.IsamError.EndOfFile => VMError.FileError,
                    else => VMError.IsamError,
                };
            };

            // Allocate buffer for the record
            const result = self.allocator.alloc(u8, len) catch return VMError.OutOfMemory;
            @memcpy(result, record_buf[0..len]);
            try self.global_buffers.append(self.allocator, result);

            try self.push(.{ .alpha = result });
        } else {
            return VMError.FileError;
        }
    }

    fn concatValues(self: *Self, a: Value, b: Value) VMError!Value {
        // Get string representations
        const a_str = switch (a) {
            .alpha => |s| s,
            .string => |s| @constCast(s),
            .integer => |i| blk: {
                const buf = self.allocator.alloc(u8, 32) catch return VMError.OutOfMemory;
                const len = std.fmt.bufPrint(buf, "{d}", .{i}) catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, buf);
                break :blk buf[0..len.len];
            },
            else => return .{ .null_val = {} },
        };

        const b_str = switch (b) {
            .alpha => |s| s,
            .string => |s| @constCast(s),
            .integer => |i| blk: {
                const buf = self.allocator.alloc(u8, 32) catch return VMError.OutOfMemory;
                const len = std.fmt.bufPrint(buf, "{d}", .{i}) catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, buf);
                break :blk buf[0..len.len];
            },
            else => return .{ .null_val = {} },
        };

        // Concatenate
        const result = self.allocator.alloc(u8, a_str.len + b_str.len) catch return VMError.OutOfMemory;
        @memcpy(result[0..a_str.len], a_str);
        @memcpy(result[a_str.len..], b_str);
        try self.global_buffers.append(self.allocator, result);

        return .{ .alpha = result };
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
