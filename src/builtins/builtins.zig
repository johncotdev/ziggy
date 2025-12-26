//! Zibol Built-in Functions and Subroutines
//!
//! Implements the standard Zibol library functions.

const std = @import("std");
const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;

pub const BuiltinError = error{
    InvalidArgument,
    OutOfMemory,
    TypeError,
};

/// Built-in function type
pub const BuiltinFn = *const fn ([]const Value, std.mem.Allocator) BuiltinError!Value;

/// Registry of built-in functions
pub const builtins = std.StaticStringMap(BuiltinFn).initComptime(.{
    // String functions
    .{ "trim", trim },
    .{ "atrim", atrim },
    .{ "ltrim", ltrim },
    .{ "len", len },
    .{ "size", size },
    .{ "instr", instr },
    .{ "upper", upper },
    .{ "lower", lower },
    .{ "string", string },

    // Numeric functions
    .{ "abs", abs },
    .{ "integer", integer },
    .{ "decimal", decimal },
    .{ "round", round },
    .{ "trunc", trunc },

    // Math functions
    .{ "sqrt", sqrt },
    .{ "sin", sin },
    .{ "cos", cos },
    .{ "tan", tan },
    .{ "log", log },
    .{ "log10", log10 },
    .{ "exp", exp },

    // Date/Time functions
    .{ "date", date },
    .{ "time", time },

    // System functions
    .{ "error", getError },
    .{ "mem", mem },
});

/// Get a built-in function by name
pub fn getBuiltin(name: []const u8) ?BuiltinFn {
    // Convert to lowercase for case-insensitive lookup
    var lower_buf: [64]u8 = undefined;
    const lower_name = std.ascii.lowerString(&lower_buf, name);
    return builtins.get(lower_name[0..@min(name.len, 64)]);
}

// ============================================================
// String Functions
// ============================================================

/// %TRIM - Remove trailing spaces
pub fn trim(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const str = args[0].toString(allocator) catch return BuiltinError.OutOfMemory;
    defer allocator.free(str);

    const trimmed = std.mem.trimRight(u8, str, " ");
    const result = allocator.dupe(u8, trimmed) catch return BuiltinError.OutOfMemory;

    return Value{ .string = result };
}

/// %ATRIM - Remove trailing spaces (alpha)
pub fn atrim(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    return trim(args, allocator);
}

/// %LTRIM - Remove leading spaces
pub fn ltrim(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const str = args[0].toString(allocator) catch return BuiltinError.OutOfMemory;
    defer allocator.free(str);

    const trimmed = std.mem.trimLeft(u8, str, " ");
    const result = allocator.dupe(u8, trimmed) catch return BuiltinError.OutOfMemory;

    return Value{ .string = result };
}

/// %LEN - String length
pub fn len(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const str = args[0].toString(allocator) catch return BuiltinError.OutOfMemory;
    defer allocator.free(str);

    return Value{ .integer = @intCast(str.len) };
}

/// %SIZE - Size of variable
pub fn size(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    return switch (args[0]) {
        .alpha => |a| Value{ .integer = @intCast(a.len) },
        .string => |s| Value{ .integer = @intCast(s.len) },
        .decimal, .integer => Value{ .integer = 8 }, // Assume 8 bytes
        else => Value{ .integer = 0 },
    };
}

/// %INSTR - Find substring
pub fn instr(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    if (args.len < 3) return BuiltinError.InvalidArgument;

    const start = args[0].toInteger();
    const haystack = args[1].toString(allocator) catch return BuiltinError.OutOfMemory;
    defer allocator.free(haystack);
    const needle = args[2].toString(allocator) catch return BuiltinError.OutOfMemory;
    defer allocator.free(needle);

    const start_pos: usize = if (start > 0) @intCast(start - 1) else 0;

    if (start_pos >= haystack.len) {
        return Value{ .integer = 0 };
    }

    if (std.mem.indexOf(u8, haystack[start_pos..], needle)) |pos| {
        return Value{ .integer = @intCast(pos + start_pos + 1) };
    }

    return Value{ .integer = 0 };
}

/// %UPPER - Convert to uppercase
pub fn upper(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const str = args[0].toString(allocator) catch return BuiltinError.OutOfMemory;
    defer allocator.free(str);

    const result = allocator.alloc(u8, str.len) catch return BuiltinError.OutOfMemory;
    for (str, 0..) |c, i| {
        result[i] = std.ascii.toUpper(c);
    }

    return Value{ .string = result };
}

/// %LOWER - Convert to lowercase
pub fn lower(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const str = args[0].toString(allocator) catch return BuiltinError.OutOfMemory;
    defer allocator.free(str);

    const result = allocator.alloc(u8, str.len) catch return BuiltinError.OutOfMemory;
    for (str, 0..) |c, i| {
        result[i] = std.ascii.toLower(c);
    }

    return Value{ .string = result };
}

/// %STRING - Convert number to string with format
pub fn string(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const value = args[0].toInteger();

    // TODO: Handle format string in args[1]
    const result = std.fmt.allocPrint(allocator, "{d}", .{value}) catch
        return BuiltinError.OutOfMemory;

    return Value{ .string = result };
}

// ============================================================
// Numeric Functions
// ============================================================

/// %ABS - Absolute value
pub fn abs(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const value = args[0].toInteger();
    return Value{ .integer = if (value < 0) -value else value };
}

/// %INTEGER - Convert to integer
pub fn integer(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    return Value{ .integer = args[0].toInteger() };
}

/// %DECIMAL - Convert to decimal
pub fn decimal(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    return Value{ .decimal = args[0].toInteger() };
}

/// %ROUND - Round to precision
pub fn round(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    // TODO: Handle precision argument
    return Value{ .integer = args[0].toInteger() };
}

/// %TRUNC - Truncate
pub fn trunc(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    return Value{ .integer = args[0].toInteger() };
}

// ============================================================
// Math Functions
// ============================================================

/// %SQRT - Square root
pub fn sqrt(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const value: f64 = @floatFromInt(args[0].toInteger());
    const result = @sqrt(value);
    return Value{ .integer = @intFromFloat(result) };
}

/// %SIN - Sine
pub fn sin(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const value: f64 = @floatFromInt(args[0].toInteger());
    const result = @sin(value);
    return Value{ .decimal = @intFromFloat(result * 1000000) }; // Store with precision
}

/// %COS - Cosine
pub fn cos(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const value: f64 = @floatFromInt(args[0].toInteger());
    const result = @cos(value);
    return Value{ .decimal = @intFromFloat(result * 1000000) };
}

/// %TAN - Tangent
pub fn tan(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const value: f64 = @floatFromInt(args[0].toInteger());
    const result = @tan(value);
    return Value{ .decimal = @intFromFloat(result * 1000000) };
}

/// %LOG - Natural logarithm
pub fn log(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const value: f64 = @floatFromInt(args[0].toInteger());
    const result = @log(value);
    return Value{ .decimal = @intFromFloat(result * 1000000) };
}

/// %LOG10 - Base-10 logarithm
pub fn log10(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const value: f64 = @floatFromInt(args[0].toInteger());
    const result = std.math.log10(value);
    return Value{ .decimal = @intFromFloat(result * 1000000) };
}

/// %EXP - Exponential
pub fn exp(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = allocator;
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const value: f64 = @floatFromInt(args[0].toInteger());
    const result = @exp(value);
    return Value{ .decimal = @intFromFloat(result * 1000000) };
}

// ============================================================
// Date/Time Functions
// ============================================================

/// %DATE - Get current date (YYYYMMDD)
pub fn date(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = args;
    _ = allocator;

    const timestamp = std.time.timestamp();
    const epoch_secs: u64 = @intCast(timestamp);
    const epoch = std.time.epoch.EpochSeconds{ .secs = epoch_secs };
    const day = epoch.getEpochDay();
    const year_day = day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();

    const date_value: i64 = @as(i64, year_day.year) * 10000 +
        @as(i64, @intFromEnum(month_day.month)) * 100 +
        @as(i64, month_day.day_index + 1);

    return Value{ .decimal = date_value };
}

/// %TIME - Get current time (HHMMSS)
pub fn time(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = args;
    _ = allocator;

    const timestamp = std.time.timestamp();
    const epoch_secs: u64 = @intCast(timestamp);
    const epoch = std.time.epoch.EpochSeconds{ .secs = epoch_secs };
    const day_secs = epoch.getDaySeconds();

    const hour = day_secs.getHoursIntoDay();
    const minute = day_secs.getMinutesIntoHour();
    const second = day_secs.getSecondsIntoMinute();

    const time_value: i64 = @as(i64, hour) * 10000 +
        @as(i64, minute) * 100 +
        @as(i64, second);

    return Value{ .decimal = time_value };
}

// ============================================================
// System Functions
// ============================================================

/// %ERROR - Get last error code
pub fn getError(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    _ = args;
    _ = allocator;
    // TODO: Track actual error codes
    return Value{ .integer = 0 };
}

/// %MEM - Allocate memory handle
pub fn mem(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    if (args.len < 1) return BuiltinError.InvalidArgument;

    const size_val = args[0].toInteger();
    if (size_val <= 0) return BuiltinError.InvalidArgument;

    const buf = allocator.alloc(u8, @intCast(size_val)) catch
        return BuiltinError.OutOfMemory;

    // Return handle (pointer as integer)
    return Value{ .handle = @intFromPtr(buf.ptr) };
}

test "builtin trim" {
    const allocator = std.testing.allocator;

    const args = [_]Value{Value{ .string = "hello   " }};
    const result = try trim(&args, allocator);
    defer allocator.free(result.string);

    try std.testing.expectEqualStrings("hello", result.string);
}

test "builtin upper" {
    const allocator = std.testing.allocator;

    const args = [_]Value{Value{ .string = "hello" }};
    const result = try upper(&args, allocator);
    defer allocator.free(result.string);

    try std.testing.expectEqualStrings("HELLO", result.string);
}

test "builtin abs" {
    const allocator = std.testing.allocator;

    const args = [_]Value{Value{ .integer = -42 }};
    const result = try abs(&args, allocator);

    try std.testing.expectEqual(@as(i64, 42), result.integer);
}
