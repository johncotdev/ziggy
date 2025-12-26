//! Zibol Standard Library
//!
//! Manages the standard library of subroutines available to all Zibol programs.
//! The stdlib combines:
//! - Native (Zig) implementations for performance-critical and system-level code
//! - Zibol implementations for higher-level utilities (once bytecode loading works)

const std = @import("std");
const SubroutineRegistry = @import("subroutines.zig").SubroutineRegistry;
const SubroutineContext = @import("subroutines.zig").SubroutineContext;
const SubroutineError = @import("subroutines.zig").SubroutineError;
const Value = @import("subroutines.zig").Value;
const Linker = @import("linker.zig").Linker;

/// Standard library manager
pub const Stdlib = struct {
    allocator: std.mem.Allocator,
    registry: *SubroutineRegistry,
    linker: Linker,
    stdlib_path: ?[]const u8,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, registry: *SubroutineRegistry) Self {
        return Self{
            .allocator = allocator,
            .registry = registry,
            .linker = Linker.init(allocator),
            .stdlib_path = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.linker.deinit();
        if (self.stdlib_path) |sp| {
            self.allocator.free(sp);
        }
    }

    /// Set the stdlib path and load standard modules
    pub fn setPath(self: *Self, path: []const u8) !void {
        if (self.stdlib_path) |old| {
            self.allocator.free(old);
        }
        self.stdlib_path = try self.allocator.dupe(u8, path);
        try self.linker.setStdlibPath(path);
    }

    /// Load all standard library modules
    pub fn loadAll(self: *Self) !void {
        // Register additional native subroutines for stdlib
        try self.registerStdlibNatives();

        // Load compiled Zibol stdlib modules if stdlib path is set
        if (self.stdlib_path) |path| {
            try self.loadZblModules(path);
        }
    }

    /// Register native stdlib subroutines
    fn registerStdlibNatives(self: *Self) !void {
        // Date/Time functions
        try self.registry.registerNative("date", native_date);
        try self.registry.registerNative("time", native_time);
        try self.registry.registerNative("datetime", native_datetime);

        // String functions
        try self.registry.registerNative("locase", native_locase);
        try self.registry.registerNative("upcase", native_upcase);
        try self.registry.registerNative("instr", native_instr);
        try self.registry.registerNative("char", native_char);

        // Math functions
        try self.registry.registerNative("abs", native_abs);
        try self.registry.registerNative("int", native_int);
        try self.registry.registerNative("frac", native_frac);

        // System functions
        try self.registry.registerNative("sleep", native_sleep);
        try self.registry.registerNative("spawn", native_spawn);

        // Memory functions
        try self.registry.registerNative("mem_alloc", native_mem_alloc);
        try self.registry.registerNative("mem_free", native_mem_free);
    }

    /// Load Zibol stdlib modules from path
    fn loadZblModules(self: *Self, path: []const u8) !void {
        // Try to open the stdlib directory
        var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch return;
        defer dir.close();

        // Iterate through .zbc files
        var iter = dir.iterate();
        while (iter.next() catch return) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".zbc")) {
                // Load the module
                var path_buf: [512]u8 = undefined;
                const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ path, entry.name }) catch continue;
                _ = self.linker.loadModuleFromPath(full_path) catch continue;
            }
        }
    }
};

// ============================================================
// Native Stdlib Subroutine Implementations
// ============================================================

/// DATE - Get current date
fn native_date(ctx: *SubroutineContext) SubroutineError!?Value {
    // Return date in YYYYMMDD format
    const ts = std.time.timestamp();
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(ts) };
    const day = epoch.getEpochDay();
    const ymd = day.calculateYearDay().calculateMonthDay();

    const result = ctx.allocator.alloc(u8, 8) catch return SubroutineError.OutOfMemory;
    _ = std.fmt.bufPrint(result, "{d:0>4}{d:0>2}{d:0>2}", .{
        ymd.year,
        @intFromEnum(ymd.month),
        ymd.day,
    }) catch return SubroutineError.OutOfMemory;

    return Value{ .alpha = result };
}

/// TIME - Get current time
fn native_time(ctx: *SubroutineContext) SubroutineError!?Value {
    const ts = std.time.timestamp();
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(ts) };
    const day_secs = epoch.getDaySeconds();

    const hours = day_secs.getHoursIntoDay();
    const mins = day_secs.getMinutesIntoHour();
    const secs = day_secs.getSecondsIntoMinute();

    const result = ctx.allocator.alloc(u8, 6) catch return SubroutineError.OutOfMemory;
    _ = std.fmt.bufPrint(result, "{d:0>2}{d:0>2}{d:0>2}", .{ hours, mins, secs }) catch return SubroutineError.OutOfMemory;

    return Value{ .alpha = result };
}

/// DATETIME - Get current date and time
fn native_datetime(ctx: *SubroutineContext) SubroutineError!?Value {
    const ts = std.time.timestamp();
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(ts) };

    const day = epoch.getEpochDay();
    const ymd = day.calculateYearDay().calculateMonthDay();
    const day_secs = epoch.getDaySeconds();

    const hours = day_secs.getHoursIntoDay();
    const mins = day_secs.getMinutesIntoHour();
    const secs = day_secs.getSecondsIntoMinute();

    const result = ctx.allocator.alloc(u8, 14) catch return SubroutineError.OutOfMemory;
    _ = std.fmt.bufPrint(result, "{d:0>4}{d:0>2}{d:0>2}{d:0>2}{d:0>2}{d:0>2}", .{
        ymd.year,
        @intFromEnum(ymd.month),
        ymd.day,
        hours,
        mins,
        secs,
    }) catch return SubroutineError.OutOfMemory;

    return Value{ .alpha = result };
}

/// LOCASE - Convert string to lowercase
fn native_locase(ctx: *SubroutineContext) SubroutineError!?Value {
    const input = ctx.getArgString(0) catch return SubroutineError.InvalidArgument;
    defer ctx.allocator.free(input);

    const result = ctx.allocator.dupe(u8, input) catch return SubroutineError.OutOfMemory;
    _ = std.ascii.lowerString(result, input);

    return Value{ .alpha = result };
}

/// UPCASE - Convert string to uppercase
fn native_upcase(ctx: *SubroutineContext) SubroutineError!?Value {
    const input = ctx.getArgString(0) catch return SubroutineError.InvalidArgument;
    defer ctx.allocator.free(input);

    const result = ctx.allocator.dupe(u8, input) catch return SubroutineError.OutOfMemory;
    _ = std.ascii.upperString(result, input);

    return Value{ .alpha = result };
}

/// INSTR - Find position of substring
fn native_instr(ctx: *SubroutineContext) SubroutineError!?Value {
    const start = ctx.getArgInt(0) catch 1;
    const haystack = ctx.getArgString(1) catch return SubroutineError.InvalidArgument;
    defer ctx.allocator.free(haystack);
    const needle = ctx.getArgString(2) catch return SubroutineError.InvalidArgument;
    defer ctx.allocator.free(needle);

    const start_idx: usize = if (start > 0) @intCast(start - 1) else 0;

    if (start_idx >= haystack.len) return Value{ .integer = 0 };

    if (std.mem.indexOf(u8, haystack[start_idx..], needle)) |pos| {
        return Value{ .integer = @intCast(pos + start_idx + 1) };
    }

    return Value{ .integer = 0 };
}

/// CHAR - Get character from code
fn native_char(ctx: *SubroutineContext) SubroutineError!?Value {
    const code = ctx.getArgInt(0) catch return SubroutineError.InvalidArgument;

    if (code < 0 or code > 255) return SubroutineError.InvalidArgument;

    var result = ctx.allocator.alloc(u8, 1) catch return SubroutineError.OutOfMemory;
    result[0] = @intCast(code);

    return Value{ .alpha = result };
}

/// ABS - Absolute value
fn native_abs(ctx: *SubroutineContext) SubroutineError!?Value {
    const val = ctx.getArgInt(0) catch return SubroutineError.InvalidArgument;
    return Value{ .integer = @intCast(@abs(val)) };
}

/// INT - Integer part
fn native_int(ctx: *SubroutineContext) SubroutineError!?Value {
    const val = ctx.getArgInt(0) catch return SubroutineError.InvalidArgument;
    return Value{ .integer = val };
}

/// FRAC - Fractional part (for implied decimals)
fn native_frac(ctx: *SubroutineContext) SubroutineError!?Value {
    const val = ctx.getArg(0) orelse return SubroutineError.InvalidArgument;

    switch (val) {
        .implied_decimal => |id| {
            const divisor = std.math.pow(i64, 10, id.precision);
            return Value{ .implied_decimal = .{
                .value = @rem(id.value, divisor),
                .precision = id.precision,
            }};
        },
        else => return Value{ .integer = 0 },
    }
}

/// SLEEP - Pause execution
fn native_sleep(ctx: *SubroutineContext) SubroutineError!?Value {
    const ms = ctx.getArgInt(0) catch return SubroutineError.InvalidArgument;
    if (ms > 0) {
        std.time.sleep(@intCast(ms * 1_000_000));
    }
    return null;
}

/// SPAWN - Execute system command
fn native_spawn(ctx: *SubroutineContext) SubroutineError!?Value {
    const cmd = ctx.getArgString(0) catch return SubroutineError.InvalidArgument;
    defer ctx.allocator.free(cmd);

    // Create null-terminated command
    const cmd_z = ctx.allocator.dupeZ(u8, cmd) catch return SubroutineError.OutOfMemory;
    defer ctx.allocator.free(cmd_z);

    // Execute via shell
    var child = std.process.Child.init(&[_][]const u8{ "/bin/sh", "-c", cmd_z }, ctx.allocator);
    child.spawn() catch return SubroutineError.FileError;
    const result = child.wait() catch return SubroutineError.FileError;

    return switch (result) {
        .Exited => |code| Value{ .integer = code },
        else => Value{ .integer = -1 },
    };
}

/// MEM_ALLOC - Allocate memory (returns handle)
fn native_mem_alloc(ctx: *SubroutineContext) SubroutineError!?Value {
    const size = ctx.getArgInt(0) catch return SubroutineError.InvalidArgument;
    if (size <= 0) return SubroutineError.InvalidArgument;

    const mem = ctx.allocator.alloc(u8, @intCast(size)) catch return SubroutineError.OutOfMemory;
    @memset(mem, 0);

    // Return pointer as handle
    return Value{ .handle = @intFromPtr(mem.ptr) };
}

/// MEM_FREE - Free allocated memory
fn native_mem_free(ctx: *SubroutineContext) SubroutineError!?Value {
    const handle = ctx.getArg(0) orelse return SubroutineError.InvalidArgument;

    switch (handle) {
        .handle => |h| {
            // Get size from allocation tracking would be needed for proper free
            // For now, this is a stub - proper implementation needs allocation tracking
            _ = h;
        },
        else => return SubroutineError.InvalidArgument,
    }

    return null;
}

test "stdlib init" {
    const allocator = std.testing.allocator;
    var registry = @import("subroutines.zig").SubroutineRegistry.init(allocator);
    defer registry.deinit();

    var stdlib = Stdlib.init(allocator, &registry);
    defer stdlib.deinit();

    try stdlib.loadAll();

    // Check that date function is registered
    const date_fn = registry.lookup("date");
    try std.testing.expect(date_fn != null);
}
