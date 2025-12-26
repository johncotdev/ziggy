//! Zibol Subroutine Registry
//!
//! Unified dispatch for XCALL subroutines - both native (Zig) and Zibol (bytecode).
//! This is the central registry for all callable subroutines in Zibol.

const std = @import("std");
const bytecode = @import("../bytecode/bytecode.zig");

// Re-export submodules
pub const channels = @import("channels.zig");
pub const linker = @import("linker.zig");
pub const stdlib = @import("stdlib.zig");

// Convenience types
pub const ChannelManager = channels.ChannelManager;
pub const Channel = channels.Channel;
pub const Linker = linker.Linker;
pub const Stdlib = stdlib.Stdlib;

/// Subroutine execution context passed to native subroutines
pub const SubroutineContext = struct {
    allocator: std.mem.Allocator,
    args: []const Value,
    channels: *ChannelManager,

    /// Get argument at index, with type coercion
    pub fn getArg(self: *const SubroutineContext, index: usize) ?Value {
        if (index >= self.args.len) return null;
        return self.args[index];
    }

    /// Get argument as string
    pub fn getArgString(self: *const SubroutineContext, index: usize) ![]u8 {
        const val = self.getArg(index) orelse return SubroutineError.InvalidArgument;
        return val.toString(self.allocator);
    }

    /// Get argument as integer
    pub fn getArgInt(self: *const SubroutineContext, index: usize) !i64 {
        const val = self.getArg(index) orelse return SubroutineError.InvalidArgument;
        return val.toInteger();
    }
};

/// Forward declarations for types from other modules
pub const Value = @import("../runtime/runtime.zig").Value;

/// Subroutine errors
pub const SubroutineError = error{
    InvalidArgument,
    OutOfMemory,
    FileError,
    NotImplemented,
    SubroutineNotFound,
};

/// Native subroutine function signature
pub const NativeSubroutineFn = *const fn (*SubroutineContext) SubroutineError!?Value;

/// Subroutine types
pub const SubroutineType = enum {
    native,     // Implemented in Zig
    bytecode,   // Compiled Zibol (from loaded module)
};

/// Subroutine definition
pub const SubroutineDef = struct {
    name: []const u8,
    sub_type: SubroutineType,
    native_fn: ?NativeSubroutineFn,
    module_index: ?u16,      // For bytecode subroutines
    routine_index: ?u16,     // For bytecode subroutines
};

/// Subroutine registry - manages all callable subroutines
pub const SubroutineRegistry = struct {
    allocator: std.mem.Allocator,
    subroutines: std.StringHashMap(SubroutineDef),
    loaded_modules: std.ArrayListAligned(*bytecode.Module, null),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        var registry = Self{
            .allocator = allocator,
            .subroutines = std.StringHashMap(SubroutineDef).init(allocator),
            .loaded_modules = .empty,
        };

        // Register built-in native subroutines
        registry.registerNativeSubroutines() catch {};

        return registry;
    }

    pub fn deinit(self: *Self) void {
        self.subroutines.deinit();
        for (self.loaded_modules.items) |mod| {
            mod.deinit();
            self.allocator.destroy(mod);
        }
        self.loaded_modules.deinit(self.allocator);
    }

    /// Register all native subroutines
    fn registerNativeSubroutines(self: *Self) !void {
        // File/ISAM subroutines
        try self.registerNative("isamc", native_isamc);
        try self.registerNative("isutl", native_isutl);

        // System subroutines
        try self.registerNative("flags", native_flags);
        try self.registerNative("getlog", native_getlog);
        try self.registerNative("setlog", native_setlog);

        // I/O subroutines
        try self.registerNative("print", native_print);
        try self.registerNative("lpque", native_lpque);

        // String subroutines
        try self.registerNative("fill", native_fill);
        try self.registerNative("copy", native_copy);

        // Memory subroutines
        try self.registerNative("s_bld", native_s_bld);
        try self.registerNative("s_parse", native_s_parse);
    }

    /// Register a native subroutine
    pub fn registerNative(self: *Self, name: []const u8, func: NativeSubroutineFn) !void {
        try self.subroutines.put(name, .{
            .name = name,
            .sub_type = .native,
            .native_fn = func,
            .module_index = null,
            .routine_index = null,
        });
    }

    /// Load a bytecode module and register its exported subroutines
    pub fn loadModule(self: *Self, path: []const u8) !void {
        const file = std.fs.cwd().openFile(path, .{}) catch {
            return SubroutineError.FileError;
        };
        defer file.close();

        const mod_ptr = try self.allocator.create(bytecode.Module);
        errdefer self.allocator.destroy(mod_ptr);

        mod_ptr.* = bytecode.Module.deserialize(self.allocator, file.reader()) catch {
            return SubroutineError.FileError;
        };

        const module_index: u16 = @intCast(self.loaded_modules.items.len);
        try self.loaded_modules.append(mod_ptr);

        // Register exported routines
        for (mod_ptr.exports, 0..) |export_entry, i| {
            if (export_entry.kind == .routine) {
                const name = switch (mod_ptr.getConstant(export_entry.name_index).?) {
                    .identifier => |n| n,
                    else => continue,
                };

                try self.subroutines.put(name, .{
                    .name = name,
                    .sub_type = .bytecode,
                    .native_fn = null,
                    .module_index = module_index,
                    .routine_index = @intCast(i),
                });
            }
        }
    }

    /// Look up a subroutine by name (case-insensitive)
    pub fn lookup(self: *Self, name: []const u8) ?SubroutineDef {
        // Convert to lowercase for lookup
        var lower_buf: [256]u8 = undefined;
        const lower_name = std.ascii.lowerString(&lower_buf, name);
        return self.subroutines.get(lower_name[0..@min(name.len, 256)]);
    }

    /// Call a subroutine by name
    pub fn call(self: *Self, name: []const u8, ctx: *SubroutineContext) SubroutineError!?Value {
        const def = self.lookup(name) orelse return SubroutineError.SubroutineNotFound;

        switch (def.sub_type) {
            .native => {
                if (def.native_fn) |func| {
                    return func(ctx);
                }
                return SubroutineError.NotImplemented;
            },
            .bytecode => {
                // TODO: Execute bytecode subroutine via VM
                return SubroutineError.NotImplemented;
            },
        }
    }
};

// ============================================================
// Native Subroutine Implementations
// ============================================================

/// ISAMC - Create an ISAM file
fn native_isamc(ctx: *SubroutineContext) SubroutineError!?Value {
    const isam = @import("../isam/isam.zig");

    if (ctx.args.len < 4) return SubroutineError.InvalidArgument;

    // Get file specification
    const file_spec = ctx.getArgString(0) catch return SubroutineError.InvalidArgument;
    defer ctx.allocator.free(file_spec);

    // Get record size
    const rec_size: u32 = @intCast(ctx.getArgInt(1) catch return SubroutineError.InvalidArgument);

    // Get number of keys
    const num_keys: usize = @intCast(ctx.getArgInt(2) catch return SubroutineError.InvalidArgument);

    if (num_keys == 0 or num_keys > 255) return SubroutineError.InvalidArgument;

    // Parse key specifications
    var key_defs = std.ArrayListAligned(isam.KeyDef, null).empty;
    defer key_defs.deinit(ctx.allocator);

    var key_segments_storage = std.ArrayListAligned([]isam.KeyDef.KeySegment, null).empty;
    defer {
        for (key_segments_storage.items) |segs| {
            ctx.allocator.free(segs);
        }
        key_segments_storage.deinit(ctx.allocator);
    }

    var key_idx: usize = 0;
    var arg_idx: usize = 3;
    while (key_idx < num_keys and arg_idx < ctx.args.len) : ({
        key_idx += 1;
        arg_idx += 1;
    }) {
        const key_spec = ctx.getArgString(arg_idx) catch continue;
        defer ctx.allocator.free(key_spec);

        const parsed = parseKeySpec(ctx.allocator, key_spec, @intCast(key_idx)) catch continue;
        key_segments_storage.append(ctx.allocator, parsed.segments) catch return SubroutineError.OutOfMemory;
        key_defs.append(ctx.allocator, .{
            .segments = parsed.segments,
            .allow_duplicates = parsed.allow_dups,
            .changes_allowed = parsed.modifiable,
            .key_number = @intCast(key_idx),
        }) catch return SubroutineError.OutOfMemory;
    }

    if (key_defs.items.len == 0) return SubroutineError.InvalidArgument;

    // Create the ISAM file
    const isam_file = isam.IsamFile.create(
        ctx.allocator,
        file_spec,
        key_defs.items,
        rec_size,
        .{},
    ) catch return SubroutineError.FileError;

    // Close it immediately (ISAMC just creates, doesn't keep open)
    isam_file.close();

    return null;
}

/// Parse key specification string
/// Supports composite keys with array syntax: START=1:16, LENGTH=8:8, TYPE=ALPHA:NOCASE
fn parseKeySpec(allocator: std.mem.Allocator, spec: []const u8, key_num: u8) !struct {
    segments: []@import("../isam/isam.zig").KeyDef.KeySegment,
    allow_dups: bool,
    modifiable: bool,
} {
    const isam = @import("../isam/isam.zig");

    // Arrays for composite key segments (max 16 segments per key)
    var starts: [16]u32 = undefined;
    var lengths: [16]u32 = undefined;
    var types: [16]isam.KeyType = undefined;
    var start_count: usize = 0;
    var length_count: usize = 0;
    var type_count: usize = 0;

    var allow_dups = false;
    var modifiable = true;
    _ = key_num;

    // Parse "START=n[:n...], LENGTH=n[:n...], TYPE=x[:x...], ..." format
    var iter = std.mem.splitAny(u8, spec, ", ");
    while (iter.next()) |part| {
        const trimmed = std.mem.trim(u8, part, " \t");
        if (trimmed.len == 0) continue;

        // Convert to uppercase for comparison
        var upper_buf: [64]u8 = undefined;
        const upper = std.ascii.upperString(&upper_buf, trimmed);

        if (std.mem.startsWith(u8, upper, "START=")) {
            // Parse array of start positions (colon-separated)
            const value_part = trimmed[6..];
            start_count = parseIntArray(value_part, &starts);
            // Convert from 1-based to 0-based indexing
            for (starts[0..start_count]) |*s| {
                if (s.* > 0) s.* -= 1;
            }
        } else if (std.mem.startsWith(u8, upper, "LENGTH=")) {
            // Parse array of lengths (colon-separated)
            const value_part = trimmed[7..];
            length_count = parseIntArray(value_part, &lengths);
        } else if (std.mem.startsWith(u8, upper, "TYPE=")) {
            // Parse array of types (colon-separated)
            const value_part = trimmed[5..];
            type_count = parseTypeArray(value_part, &types);
        } else if (std.mem.eql(u8, upper[0..@min(upper.len, 4)], "DUPS")) {
            allow_dups = true;
        } else if (std.mem.eql(u8, upper[0..@min(upper.len, 8)], "NOMODIFY")) {
            modifiable = false;
        }
    }

    // Determine segment count (use max of start/length counts, default to 1)
    var seg_count = @max(start_count, length_count);
    if (seg_count == 0) seg_count = 1;

    if (seg_count > 0 and length_count == 0) return error.InvalidArgument;

    // Validate that we have matching counts or can extend with defaults
    // If lengths provided but starts missing, error
    if (length_count > 0 and start_count == 0) {
        // Default start positions (consecutive from 0)
        var pos: u32 = 0;
        for (0..length_count) |i| {
            starts[i] = pos;
            pos += lengths[i];
        }
        start_count = length_count;
    }

    // Create segments
    const segments = try allocator.alloc(isam.KeyDef.KeySegment, seg_count);
    errdefer allocator.free(segments);

    for (0..seg_count) |i| {
        segments[i] = .{
            .start = if (i < start_count) starts[i] else 0,
            .length = if (i < length_count) lengths[i] else 0,
            .key_type = if (i < type_count) types[i] else .alpha,
        };

        // Validate length is non-zero
        if (segments[i].length == 0) {
            allocator.free(segments);
            return error.InvalidArgument;
        }
    }

    return .{
        .segments = segments,
        .allow_dups = allow_dups,
        .modifiable = modifiable,
    };
}

/// Parse a colon-separated list of integers
fn parseIntArray(value: []const u8, out: []u32) usize {
    var count: usize = 0;
    var iter = std.mem.splitScalar(u8, value, ':');
    while (iter.next()) |num_str| {
        if (count >= out.len) break;
        const trimmed = std.mem.trim(u8, num_str, " \t");
        out[count] = std.fmt.parseInt(u32, trimmed, 10) catch 0;
        count += 1;
    }
    return count;
}

/// Parse a colon-separated list of key types
fn parseTypeArray(value: []const u8, out: []@import("../isam/isam.zig").KeyType) usize {
    const isam = @import("../isam/isam.zig");
    var count: usize = 0;
    var iter = std.mem.splitScalar(u8, value, ':');
    while (iter.next()) |type_str| {
        if (count >= out.len) break;
        const trimmed = std.mem.trim(u8, type_str, " \t");
        var upper_buf: [16]u8 = undefined;
        const upper = std.ascii.upperString(&upper_buf, trimmed);

        out[count] = if (std.mem.eql(u8, upper[0..@min(upper.len, 6)], "NOCASE"))
            isam.KeyType.nocase
        else if (std.mem.eql(u8, upper[0..@min(upper.len, 7)], "DECIMAL"))
            isam.KeyType.decimal
        else if (std.mem.eql(u8, upper[0..@min(upper.len, 7)], "INTEGER"))
            isam.KeyType.integer
        else if (std.mem.eql(u8, upper[0..@min(upper.len, 6)], "PACKED"))
            isam.KeyType.packed_decimal
        else
            isam.KeyType.alpha;

        count += 1;
    }
    return count;
}

/// ISUTL - ISAM utility functions
fn native_isutl(ctx: *SubroutineContext) SubroutineError!?Value {
    _ = ctx;
    // TODO: Implement ISAM utilities
    return null;
}

/// FLAGS - Set runtime flags
fn native_flags(ctx: *SubroutineContext) SubroutineError!?Value {
    _ = ctx;
    // TODO: Implement flags
    return null;
}

/// GETLOG - Get logical name
fn native_getlog(ctx: *SubroutineContext) SubroutineError!?Value {
    if (ctx.args.len < 1) return SubroutineError.InvalidArgument;

    const name = ctx.getArgString(0) catch return SubroutineError.InvalidArgument;
    defer ctx.allocator.free(name);

    // Get environment variable
    const value = std.process.getEnvVarOwned(ctx.allocator, name) catch {
        return Value{ .string = "" };
    };

    return Value{ .string = value };
}

/// SETLOG - Set logical name (environment variable)
fn native_setlog(ctx: *SubroutineContext) SubroutineError!?Value {
    _ = ctx;
    // Environment variables can't be set in this process to affect it
    return null;
}

/// PRINT - Print to printer
fn native_print(ctx: *SubroutineContext) SubroutineError!?Value {
    _ = ctx;
    // TODO: Implement printer output
    return null;
}

/// LPQUE - Queue to printer
fn native_lpque(ctx: *SubroutineContext) SubroutineError!?Value {
    _ = ctx;
    // TODO: Implement print queue
    return null;
}

/// FILL - Fill string with character
fn native_fill(ctx: *SubroutineContext) SubroutineError!?Value {
    if (ctx.args.len < 2) return SubroutineError.InvalidArgument;

    const fill_char_val = ctx.getArgString(0) catch return SubroutineError.InvalidArgument;
    defer ctx.allocator.free(fill_char_val);

    const size = ctx.getArgInt(1) catch return SubroutineError.InvalidArgument;
    if (size <= 0) return SubroutineError.InvalidArgument;

    const fill_char: u8 = if (fill_char_val.len > 0) fill_char_val[0] else ' ';
    const result = ctx.allocator.alloc(u8, @intCast(size)) catch return SubroutineError.OutOfMemory;
    @memset(result, fill_char);

    return Value{ .alpha = result };
}

/// COPY - Copy string
fn native_copy(ctx: *SubroutineContext) SubroutineError!?Value {
    if (ctx.args.len < 1) return SubroutineError.InvalidArgument;

    const src = ctx.getArgString(0) catch return SubroutineError.InvalidArgument;
    defer ctx.allocator.free(src);

    const result = ctx.allocator.dupe(u8, src) catch return SubroutineError.OutOfMemory;

    return Value{ .alpha = result };
}

/// S_BLD - Build a string with substitution
fn native_s_bld(ctx: *SubroutineContext) SubroutineError!?Value {
    _ = ctx;
    // TODO: Implement string building
    return SubroutineError.NotImplemented;
}

/// S_PARSE - Parse a string
fn native_s_parse(ctx: *SubroutineContext) SubroutineError!?Value {
    _ = ctx;
    // TODO: Implement string parsing
    return SubroutineError.NotImplemented;
}

test "subroutine registry init" {
    const allocator = std.testing.allocator;
    var registry = SubroutineRegistry.init(allocator);
    defer registry.deinit();

    // ISAMC should be registered
    const isamc = registry.lookup("isamc");
    try std.testing.expect(isamc != null);
    try std.testing.expectEqual(SubroutineType.native, isamc.?.sub_type);
}

test "parseKeySpec single segment" {
    const allocator = std.testing.allocator;

    // Simple single segment key
    const result = try parseKeySpec(allocator, "START=1, LENGTH=8, TYPE=ALPHA", 0);
    defer allocator.free(result.segments);

    try std.testing.expectEqual(@as(usize, 1), result.segments.len);
    try std.testing.expectEqual(@as(u32, 0), result.segments[0].start); // 1-based to 0-based
    try std.testing.expectEqual(@as(u32, 8), result.segments[0].length);
    try std.testing.expect(!result.allow_dups);
    try std.testing.expect(result.modifiable);
}

test "parseKeySpec composite key" {
    const allocator = std.testing.allocator;

    // Composite key with two segments
    const result = try parseKeySpec(allocator, "START=1:16, LENGTH=8:8, TYPE=ALPHA:NOCASE", 0);
    defer allocator.free(result.segments);

    try std.testing.expectEqual(@as(usize, 2), result.segments.len);

    // First segment: position 0, length 8, alpha
    try std.testing.expectEqual(@as(u32, 0), result.segments[0].start);
    try std.testing.expectEqual(@as(u32, 8), result.segments[0].length);
    try std.testing.expectEqual(@import("../isam/isam.zig").KeyType.alpha, result.segments[0].key_type);

    // Second segment: position 15, length 8, nocase
    try std.testing.expectEqual(@as(u32, 15), result.segments[1].start);
    try std.testing.expectEqual(@as(u32, 8), result.segments[1].length);
    try std.testing.expectEqual(@import("../isam/isam.zig").KeyType.nocase, result.segments[1].key_type);
}

test "parseKeySpec with dups and nomodify" {
    const allocator = std.testing.allocator;

    const result = try parseKeySpec(allocator, "START=1, LENGTH=10, DUPS, NOMODIFY", 1);
    defer allocator.free(result.segments);

    try std.testing.expectEqual(@as(usize, 1), result.segments.len);
    try std.testing.expect(result.allow_dups);
    try std.testing.expect(!result.modifiable);
}
