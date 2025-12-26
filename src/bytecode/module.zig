//! Ziggy DBL Bytecode Module Format
//!
//! Defines the structure of compiled bytecode modules (.zbc files).

const std = @import("std");
const Opcode = @import("opcodes.zig").Opcode;

/// Magic numbers for file identification
pub const MAGIC_MODULE = [4]u8{ 'Z', 'B', 'C', '1' };
pub const MAGIC_LIBRARY = [4]u8{ 'Z', 'B', 'L', '1' };
pub const MAGIC_EXECUTABLE = [4]u8{ 'Z', 'B', 'X', '1' };

/// Current bytecode version
pub const VERSION_MAJOR: u16 = 0;
pub const VERSION_MINOR: u16 = 1;

/// Module flags
pub const ModuleFlags = packed struct(u32) {
    has_debug_info: bool = false,
    has_native_extensions: bool = false,
    requires_isam: bool = false,
    is_library: bool = false,
    _reserved: u28 = 0,
};

/// Section types
pub const SectionType = enum(u32) {
    constants = 0x0001,
    types = 0x0002,
    routines = 0x0003,
    code = 0x0004,
    exports = 0x0005,
    imports = 0x0006,
    debug = 0x0007,
    native = 0x0008,
};

/// Module header (32 bytes)
pub const ModuleHeader = extern struct {
    magic: [4]u8,
    version_major: u16,
    version_minor: u16,
    flags: ModuleFlags,
    section_count: u32,
    entry_point: u32, // 0xFFFFFFFF if library
    source_hash: u32,
    _reserved: [8]u8,

    pub fn init() ModuleHeader {
        return .{
            .magic = MAGIC_MODULE,
            .version_major = VERSION_MAJOR,
            .version_minor = VERSION_MINOR,
            .flags = .{},
            .section_count = 0,
            .entry_point = 0xFFFFFFFF,
            .source_hash = 0,
            ._reserved = [_]u8{0} ** 8,
        };
    }

    pub fn isValid(self: *const ModuleHeader) bool {
        return std.mem.eql(u8, &self.magic, &MAGIC_MODULE);
    }
};

/// Section table entry
pub const SectionEntry = extern struct {
    section_type: SectionType,
    offset: u32,
    size: u32,
    entry_count: u32,
};

/// Constant pool entry tags
pub const ConstantTag = enum(u8) {
    integer = 0x01,
    decimal = 0x02, // i64 + u8 precision
    string = 0x03,
    alpha = 0x04, // fixed-size, space-padded
    identifier = 0x05,
    record_ref = 0x06,
    routine_ref = 0x07,
};

/// Constant pool value
pub const Constant = union(ConstantTag) {
    integer: i64,
    decimal: struct { value: i64, precision: u8 },
    string: []const u8,
    alpha: struct { data: []const u8, size: u16 },
    identifier: []const u8,
    record_ref: u16,
    routine_ref: u16,

    /// Serialize constant to writer
    pub fn serialize(self: Constant, writer: anytype) !void {
        // Write tag
        try writer.writeByte(@intFromEnum(std.meta.activeTag(self)));

        switch (self) {
            .integer => |val| try writer.writeInt(i64, val, .little),
            .decimal => |d| {
                try writer.writeInt(i64, d.value, .little);
                try writer.writeByte(d.precision);
            },
            .string => |s| {
                try writer.writeInt(u32, @intCast(s.len), .little);
                try writer.writeAll(s);
            },
            .alpha => |a| {
                try writer.writeInt(u16, a.size, .little);
                try writer.writeAll(a.data[0..a.size]);
            },
            .identifier => |id| {
                try writer.writeInt(u32, @intCast(id.len), .little);
                try writer.writeAll(id);
            },
            .record_ref => |r| try writer.writeInt(u16, r, .little),
            .routine_ref => |r| try writer.writeInt(u16, r, .little),
        }
    }

    /// Deserialize constant from reader
    pub fn deserialize(allocator: std.mem.Allocator, reader: anytype) !Constant {
        const tag = try reader.readByte();
        const tag_enum: ConstantTag = @enumFromInt(tag);

        return switch (tag_enum) {
            .integer => .{ .integer = try reader.readInt(i64, .little) },
            .decimal => .{
                .decimal = .{
                    .value = try reader.readInt(i64, .little),
                    .precision = try reader.readByte(),
                },
            },
            .string => blk: {
                const len = try reader.readInt(u32, .little);
                const str = try allocator.alloc(u8, len);
                try reader.readNoEof(str);
                break :blk .{ .string = str };
            },
            .alpha => blk: {
                const size = try reader.readInt(u16, .little);
                const data = try allocator.alloc(u8, size);
                try reader.readNoEof(data);
                break :blk .{ .alpha = .{ .data = data, .size = size } };
            },
            .identifier => blk: {
                const len = try reader.readInt(u32, .little);
                const id = try allocator.alloc(u8, len);
                try reader.readNoEof(id);
                break :blk .{ .identifier = id };
            },
            .record_ref => .{ .record_ref = try reader.readInt(u16, .little) },
            .routine_ref => .{ .routine_ref = try reader.readInt(u16, .little) },
        };
    }
};

/// Data type codes
pub const DataTypeCode = enum(u8) {
    alpha = 0x01,
    decimal = 0x02,
    implied_decimal = 0x03,
    integer1 = 0x04,
    integer2 = 0x05,
    integer4 = 0x06,
    integer8 = 0x07,
    structure = 0x08,
    handle = 0x09,
    group = 0x0A,
};

/// Field definition
pub const FieldDef = struct {
    name_index: u16, // Index into constant pool
    data_type: DataTypeCode,
    flags: u8,
    offset: u16,
    size: u16,
    precision: u8,
    array_dims: []u16,
};

/// Record/type definition
pub const TypeDef = struct {
    type_id: u16,
    kind: TypeKind,
    flags: u8,
    name_index: u16,
    total_size: u32,
    fields: []FieldDef,

    pub const TypeKind = enum(u8) {
        record = 0,
        group = 1,
        alias = 2,
    };
};

/// Routine flags
pub const RoutineFlags = packed struct(u16) {
    is_public: bool = false,
    is_function: bool = false,
    is_subroutine: bool = false,
    has_varargs: bool = false,
    is_entry_point: bool = false,
    _reserved: u11 = 0,
};

/// Parameter pass mode
pub const ParamMode = enum(u8) {
    in = 0,
    out = 1,
    inout = 2,
};

/// Parameter definition
pub const ParamDef = struct {
    name_index: u16,
    data_type: DataTypeCode,
    mode: ParamMode,
    default_value: ?u16, // Constant pool index
};

/// Routine definition
pub const RoutineDef = struct {
    name_index: u16,
    flags: RoutineFlags,
    code_offset: u32,
    code_length: u32,
    param_count: u16,
    local_count: u16,
    max_stack: u16,
    params: []ParamDef,
};

/// Export entry
pub const ExportEntry = struct {
    name_index: u16,
    kind: ExportKind,
    flags: u8,
    index: u16,

    pub const ExportKind = enum(u8) {
        routine = 0,
        record = 1,
        global = 2,
    };
};

/// Import entry
pub const ImportEntry = struct {
    name_index: u16,
    module_index: u16, // 0 = any module
    kind: ImportKind,
    flags: ImportFlags,

    pub const ImportKind = enum(u8) {
        routine = 0,
        record = 1,
        global = 2,
    };

    pub const ImportFlags = packed struct(u8) {
        required: bool = true,
        _reserved: u7 = 0,
    };
};

/// Debug line entry
pub const LineEntry = struct {
    code_offset: u32,
    line: u16,
    column: u16,
};

/// Debug local variable entry
pub const LocalVarEntry = struct {
    name_index: u16,
    slot: u16,
    scope_start: u32,
    scope_end: u32,
    data_type: DataTypeCode,
};

/// Complete module structure
pub const Module = struct {
    allocator: std.mem.Allocator,
    header: ModuleHeader,
    constants: []Constant,
    types: []TypeDef,
    routines: []RoutineDef,
    code: []u8,
    exports: []ExportEntry,
    imports: []ImportEntry,

    // Debug info (optional)
    source_file: ?[]const u8,
    line_table: ?[]LineEntry,
    local_vars: ?[]LocalVarEntry,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .header = ModuleHeader.init(),
            .constants = &[_]Constant{},
            .types = &[_]TypeDef{},
            .routines = &[_]RoutineDef{},
            .code = &[_]u8{},
            .exports = &[_]ExportEntry{},
            .imports = &[_]ImportEntry{},
            .source_file = null,
            .line_table = null,
            .local_vars = null,
        };
    }

    pub fn deinit(self: *Self) void {
        // Free allocated slices
        if (self.constants.len > 0) {
            for (self.constants) |c| {
                switch (c) {
                    .string => |s| self.allocator.free(s),
                    .alpha => |a| self.allocator.free(a.data),
                    .identifier => |s| self.allocator.free(s),
                    else => {},
                }
            }
            self.allocator.free(self.constants);
        }
        if (self.types.len > 0) self.allocator.free(self.types);
        if (self.routines.len > 0) self.allocator.free(self.routines);
        if (self.code.len > 0) self.allocator.free(self.code);
        if (self.exports.len > 0) self.allocator.free(self.exports);
        if (self.imports.len > 0) self.allocator.free(self.imports);
        if (self.line_table) |lt| self.allocator.free(lt);
        if (self.local_vars) |lv| self.allocator.free(lv);
        if (self.source_file) |sf| self.allocator.free(sf);
    }

    /// Serialize module to bytes
    pub fn serialize(self: *const Self, writer: anytype) !void {
        // Write header
        try writer.writeAll(std.mem.asBytes(&self.header));

        // Write constants section
        try writer.writeInt(u32, @intCast(self.constants.len), .little);
        for (self.constants) |constant| {
            try constant.serialize(writer);
        }

        // Write code section
        try writer.writeInt(u32, @intCast(self.code.len), .little);
        try writer.writeAll(self.code);
    }

    /// Deserialize module from bytes
    pub fn deserialize(allocator: std.mem.Allocator, reader: anytype) !Self {
        var module = Self.init(allocator);

        // Read header
        const header_bytes = try reader.readBytesNoEof(@sizeOf(ModuleHeader));
        module.header = std.mem.bytesToValue(ModuleHeader, &header_bytes);

        if (!module.header.isValid()) {
            return error.InvalidModule;
        }

        // Read constants section
        const const_count = try reader.readInt(u32, .little);
        if (const_count > 0) {
            module.constants = try allocator.alloc(Constant, const_count);
            for (0..const_count) |i| {
                module.constants[i] = try Constant.deserialize(allocator, reader);
            }
        }

        // Read code section
        const code_len = try reader.readInt(u32, .little);
        if (code_len > 0) {
            module.code = try allocator.alloc(u8, code_len);
            try reader.readNoEof(module.code);
        }

        return module;
    }

    /// Get a constant by index
    pub fn getConstant(self: *const Self, index: u16) ?Constant {
        if (index >= self.constants.len) return null;
        return self.constants[index];
    }

    /// Get a type definition by index
    pub fn getType(self: *const Self, index: u16) ?*const TypeDef {
        if (index >= self.types.len) return null;
        return &self.types[index];
    }

    /// Get a routine definition by index
    pub fn getRoutine(self: *const Self, index: u16) ?*const RoutineDef {
        if (index >= self.routines.len) return null;
        return &self.routines[index];
    }

    /// Find routine by name
    pub fn findRoutine(self: *const Self, name: []const u8) ?u16 {
        for (self.routines, 0..) |routine, i| {
            if (self.getConstant(routine.name_index)) |c| {
                switch (c) {
                    .identifier => |n| {
                        if (std.mem.eql(u8, n, name)) {
                            return @intCast(i);
                        }
                    },
                    else => {},
                }
            }
        }
        return null;
    }
};

/// Library bundle (collection of modules)
pub const Library = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    modules: []Module,
    master_exports: []MasterExport,

    pub const MasterExport = struct {
        name_index: u16,
        module_index: u16,
        local_index: u16,
    };

    pub fn init(allocator: std.mem.Allocator, name: []const u8) Library {
        return .{
            .allocator = allocator,
            .name = name,
            .modules = &[_]Module{},
            .master_exports = &[_]MasterExport{},
        };
    }

    pub fn deinit(self: *Library) void {
        for (self.modules) |*m| {
            m.deinit();
        }
        if (self.modules.len > 0) self.allocator.free(self.modules);
        if (self.master_exports.len > 0) self.allocator.free(self.master_exports);
    }
};

test "module header" {
    const header = ModuleHeader.init();
    try std.testing.expect(header.isValid());
    try std.testing.expectEqual(VERSION_MAJOR, header.version_major);
    try std.testing.expectEqual(VERSION_MINOR, header.version_minor);
}
