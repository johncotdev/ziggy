//! Ziggy DBL Subroutine Registry
//!
//! Unified dispatch for XCALL subroutines - both native (Zig) and DBL (bytecode).
//! This is the central registry for all callable subroutines in Ziggy DBL.

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
    bytecode,   // Compiled DBL (from loaded module)
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
fn parseKeySpec(allocator: std.mem.Allocator, spec: []const u8, key_num: u8) !struct {
    segments: []@import("../isam/isam.zig").KeyDef.KeySegment,
    allow_dups: bool,
    modifiable: bool,
} {
    const isam = @import("../isam/isam.zig");

    var start: u16 = 0;
    var length: u16 = 0;
    var allow_dups = false;
    var modifiable = true;
    _ = key_num;

    // Parse "START=n, LENGTH=n, ..." format
    var iter = std.mem.splitAny(u8, spec, ", ");
    while (iter.next()) |part| {
        const trimmed = std.mem.trim(u8, part, " \t");
        if (trimmed.len == 0) continue;

        if (std.mem.startsWith(u8, trimmed, "START=")) {
            start = std.fmt.parseInt(u16, trimmed[6..], 10) catch 0;
            if (start > 0) start -= 1; // Convert to 0-based
        } else if (std.mem.startsWith(u8, trimmed, "LENGTH=")) {
            length = std.fmt.parseInt(u16, trimmed[7..], 10) catch 0;
        } else if (std.mem.eql(u8, std.ascii.upperString(@constCast(trimmed), trimmed), "DUPS")) {
            allow_dups = true;
        } else if (std.mem.eql(u8, std.ascii.upperString(@constCast(trimmed), trimmed), "NOMODIFY")) {
            modifiable = false;
        }
    }

    if (length == 0) return error.InvalidArgument;

    // Create single segment
    const segments = try allocator.alloc(isam.KeyDef.KeySegment, 1);
    segments[0] = .{
        .start = start,
        .length = length,
        .key_type = .alpha,
    };

    return .{
        .segments = segments,
        .allow_dups = allow_dups,
        .modifiable = modifiable,
    };
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
