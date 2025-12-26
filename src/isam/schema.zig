//! ZiggyDB Schema - Self-describing database metadata
//!
//! Stores table and field definitions within the .zdb file, enabling:
//! - Runtime introspection of database structure
//! - .NET interop with auto-generated record types
//! - Multi-table support with tag byte discrimination
//! - Schema versioning and evolution
//!
//! Multi-table Pattern:
//! A single ZiggyDB file can contain multiple record types (tables),
//! distinguished by a tag byte at a fixed position (typically byte 0).
//! All tables share the same key structure but have different field layouts.

const std = @import("std");

/// Maximum lengths for names
pub const MAX_NAME_LEN: usize = 64;
pub const MAX_FIELDS_PER_TABLE: usize = 256;
pub const MAX_TABLES_PER_SCHEMA: usize = 64;

/// Field data types (compatible with Zibol types)
pub const FieldType = enum(u8) {
    alpha = 0, // ,a - Character string (space-padded)
    decimal = 1, // ,d - Implied decimal (ASCII digits)
    integer = 2, // ,i - Binary integer (signed)
    packed_decimal = 3, // ,p - Packed decimal (BCD)

    // Extended types for .NET interop
    date = 10, // Date stored as YYYYMMDD decimal
    time = 11, // Time stored as HHMMSS decimal
    datetime = 12, // Combined date/time
    boolean = 13, // Single byte 0/1
    binary = 14, // Raw bytes (no conversion)

    pub fn toChar(self: FieldType) u8 {
        return switch (self) {
            .alpha => 'a',
            .decimal => 'd',
            .integer => 'i',
            .packed_decimal => 'p',
            .date => 'D',
            .time => 'T',
            .datetime => 't',
            .boolean => 'b',
            .binary => 'x',
        };
    }

    pub fn fromChar(c: u8) ?FieldType {
        return switch (c) {
            'a', 'A' => .alpha,
            'd' => .decimal,
            'D' => .date,
            'i', 'I' => .integer,
            'p', 'P' => .packed_decimal,
            'b', 'B' => .boolean,
            'x', 'X' => .binary,
            't' => .datetime,
            'T' => .time,
            else => null,
        };
    }
};

/// Field definition within a table
pub const FieldDef = struct {
    /// Field name (e.g., "customer_id")
    name: []const u8,

    /// Data type
    field_type: FieldType,

    /// Start position in record (0-based)
    position: u32,

    /// Length in bytes
    length: u32,

    /// Decimal places (for decimal type)
    decimal_places: u8,

    /// Field flags
    flags: FieldFlags,

    pub const FieldFlags = packed struct(u8) {
        /// Field is part of primary key
        is_key: bool = false,
        /// Field allows null/empty
        nullable: bool = false,
        /// Field has default value
        has_default: bool = false,
        /// Reserved
        _reserved: u5 = 0,
    };

    /// Calculate the .NET CLR type name for this field
    pub fn dotnetType(self: FieldDef) []const u8 {
        return switch (self.field_type) {
            .alpha => "string",
            .decimal => if (self.decimal_places > 0) "decimal" else "long",
            .integer => switch (self.length) {
                1 => "sbyte",
                2 => "short",
                4 => "int",
                8 => "long",
                else => "long",
            },
            .packed_decimal => "decimal",
            .date => "DateOnly",
            .time => "TimeOnly",
            .datetime => "DateTime",
            .boolean => "bool",
            .binary => "byte[]",
        };
    }

    /// Serialize field definition to bytes
    pub fn serialize(self: FieldDef, writer: anytype) !void {
        // Name length + name
        const name_len: u8 = @intCast(self.name.len);
        try writer.writeByte(name_len);
        try writer.writeAll(self.name);

        // Type
        try writer.writeByte(@intFromEnum(self.field_type));

        // Position (4 bytes, little endian)
        try writer.writeInt(u32, self.position, .little);

        // Length (4 bytes)
        try writer.writeInt(u32, self.length, .little);

        // Decimal places
        try writer.writeByte(self.decimal_places);

        // Flags
        try writer.writeByte(@as(u8, @bitCast(self.flags)));
    }

    /// Deserialize field definition from bytes
    pub fn deserialize(reader: anytype, allocator: std.mem.Allocator) !FieldDef {
        // Name
        const name_len = try reader.readByte();
        const name = try allocator.alloc(u8, name_len);
        _ = try reader.readAll(name);

        // Type
        const type_byte = try reader.readByte();
        const field_type: FieldType = @enumFromInt(type_byte);

        // Position
        const position = try reader.readInt(u32, .little);

        // Length
        const length = try reader.readInt(u32, .little);

        // Decimal places
        const decimal_places = try reader.readByte();

        // Flags
        const flags_byte = try reader.readByte();
        const flags: FieldDef.FieldFlags = @bitCast(flags_byte);

        return FieldDef{
            .name = name,
            .field_type = field_type,
            .position = position,
            .length = length,
            .decimal_places = decimal_places,
            .flags = flags,
        };
    }
};

/// Table definition (one record type within the ISAM file)
pub const TableDef = struct {
    /// Table name (e.g., "order_header")
    name: []const u8,

    /// Tag byte value that identifies this table type
    /// Tag position is defined at schema level
    /// Use 0 for single-table files (no tag)
    tag: u8,

    /// Total record size for this table
    record_size: u32,

    /// Field definitions
    fields: []FieldDef,

    /// Table flags
    flags: TableFlags,

    pub const TableFlags = packed struct(u8) {
        /// This is the default table (no tag matching required)
        is_default: bool = false,
        /// Reserved
        _reserved: u7 = 0,
    };

    /// Get field by name
    pub fn getField(self: TableDef, name: []const u8) ?*const FieldDef {
        for (self.fields) |*field| {
            if (std.mem.eql(u8, field.name, name)) {
                return field;
            }
        }
        return null;
    }

    /// Serialize table definition
    pub fn serialize(self: TableDef, writer: anytype) !void {
        // Name length + name
        const name_len: u8 = @intCast(self.name.len);
        try writer.writeByte(name_len);
        try writer.writeAll(self.name);

        // Tag byte
        try writer.writeByte(self.tag);

        // Record size
        try writer.writeInt(u32, self.record_size, .little);

        // Flags
        try writer.writeByte(@as(u8, @bitCast(self.flags)));

        // Field count + fields
        const field_count: u16 = @intCast(self.fields.len);
        try writer.writeInt(u16, field_count, .little);

        for (self.fields) |field| {
            try field.serialize(writer);
        }
    }

    /// Deserialize table definition
    pub fn deserialize(reader: anytype, allocator: std.mem.Allocator) !TableDef {
        // Name
        const name_len = try reader.readByte();
        const name = try allocator.alloc(u8, name_len);
        _ = try reader.readAll(name);

        // Tag
        const tag = try reader.readByte();

        // Record size
        const record_size = try reader.readInt(u32, .little);

        // Flags
        const flags_byte = try reader.readByte();
        const flags: TableDef.TableFlags = @bitCast(flags_byte);

        // Fields
        const field_count = try reader.readInt(u16, .little);
        const fields = try allocator.alloc(FieldDef, field_count);

        for (fields) |*field| {
            field.* = try FieldDef.deserialize(reader, allocator);
        }

        return TableDef{
            .name = name,
            .tag = tag,
            .record_size = record_size,
            .fields = fields,
            .flags = flags,
        };
    }

    pub fn deinit(self: *TableDef, allocator: std.mem.Allocator) void {
        for (self.fields) |*field| {
            allocator.free(field.name);
        }
        allocator.free(self.fields);
        allocator.free(self.name);
    }
};

/// Schema - complete database structure definition
pub const Schema = struct {
    /// Schema version (for evolution)
    version: u32,

    /// Database description
    description: []const u8,

    /// Position of tag byte in records (typically 0)
    /// Set to 0xFFFFFFFF if no tag byte (single-table)
    tag_position: u32,

    /// Table definitions
    tables: []TableDef,

    /// Schema flags
    flags: SchemaFlags,

    pub const SchemaFlags = packed struct(u16) {
        /// Schema has been modified since creation
        modified: bool = false,
        /// Enable strict type checking
        strict_types: bool = true,
        /// Reserved
        _reserved: u14 = 0,
    };

    pub const MAGIC = [4]u8{ 'Z', 'S', 'C', 'H' }; // Ziggy SCHema
    pub const CURRENT_VERSION: u32 = 1;
    pub const NO_TAG: u32 = 0xFFFFFFFF;

    /// Create a new empty schema
    pub fn init(allocator: std.mem.Allocator) !*Schema {
        const schema = try allocator.create(Schema);
        schema.* = .{
            .version = CURRENT_VERSION,
            .description = "",
            .tag_position = NO_TAG,
            .tables = &[_]TableDef{},
            .flags = .{},
        };
        return schema;
    }

    /// Check if this is a multi-table schema
    pub fn isMultiTable(self: Schema) bool {
        return self.tag_position != NO_TAG and self.tables.len > 1;
    }

    /// Get table by tag byte
    pub fn getTableByTag(self: Schema, tag: u8) ?*const TableDef {
        for (self.tables) |*table| {
            if (table.tag == tag) {
                return table;
            }
        }
        // Return default table if exists
        for (self.tables) |*table| {
            if (table.flags.is_default) {
                return table;
            }
        }
        return null;
    }

    /// Get table by name
    pub fn getTableByName(self: Schema, name: []const u8) ?*const TableDef {
        for (self.tables) |*table| {
            if (std.mem.eql(u8, table.name, name)) {
                return table;
            }
        }
        return null;
    }

    /// Get the default (or only) table
    pub fn getDefaultTable(self: Schema) ?*const TableDef {
        if (self.tables.len == 0) return null;
        // Look for explicit default
        for (self.tables) |*table| {
            if (table.flags.is_default) return table;
        }
        // Return first table if single-table
        if (self.tables.len == 1) return &self.tables[0];
        return null;
    }

    /// Serialize entire schema to bytes
    pub fn serialize(self: Schema, writer: anytype) !void {
        // Magic
        try writer.writeAll(&MAGIC);

        // Version
        try writer.writeInt(u32, self.version, .little);

        // Description length + description
        const desc_len: u16 = @intCast(self.description.len);
        try writer.writeInt(u16, desc_len, .little);
        try writer.writeAll(self.description);

        // Tag position
        try writer.writeInt(u32, self.tag_position, .little);

        // Flags
        try writer.writeInt(u16, @as(u16, @bitCast(self.flags)), .little);

        // Table count + tables
        const table_count: u16 = @intCast(self.tables.len);
        try writer.writeInt(u16, table_count, .little);

        for (self.tables) |table| {
            try table.serialize(writer);
        }
    }

    /// Calculate serialized size
    pub fn serializedSize(self: Schema) usize {
        var size: usize = 4 + 4 + 2 + self.description.len + 4 + 2 + 2; // header

        for (self.tables) |table| {
            size += 1 + table.name.len + 1 + 4 + 1 + 2; // table header
            for (table.fields) |field| {
                size += 1 + field.name.len + 1 + 4 + 4 + 1 + 1; // field
            }
        }

        return size;
    }

    /// Deserialize schema from bytes
    pub fn deserialize(reader: anytype, allocator: std.mem.Allocator) !*Schema {
        // Magic
        var magic: [4]u8 = undefined;
        _ = try reader.readAll(&magic);
        if (!std.mem.eql(u8, &magic, &MAGIC)) {
            return error.InvalidSchema;
        }

        // Version
        const version = try reader.readInt(u32, .little);

        // Description
        const desc_len = try reader.readInt(u16, .little);
        const description = try allocator.alloc(u8, desc_len);
        _ = try reader.readAll(description);

        // Tag position
        const tag_position = try reader.readInt(u32, .little);

        // Flags
        const flags_int = try reader.readInt(u16, .little);
        const flags: SchemaFlags = @bitCast(flags_int);

        // Tables
        const table_count = try reader.readInt(u16, .little);
        const tables = try allocator.alloc(TableDef, table_count);

        for (tables) |*table| {
            table.* = try TableDef.deserialize(reader, allocator);
        }

        const schema = try allocator.create(Schema);
        schema.* = .{
            .version = version,
            .description = description,
            .tag_position = tag_position,
            .tables = tables,
            .flags = flags,
        };

        return schema;
    }

    pub fn deinit(self: *Schema, allocator: std.mem.Allocator) void {
        if (self.description.len > 0) {
            allocator.free(self.description);
        }
        for (self.tables) |*table| {
            table.deinit(allocator);
        }
        if (self.tables.len > 0) {
            allocator.free(self.tables);
        }
        allocator.destroy(self);
    }
};

/// Builder for constructing schemas programmatically
pub const SchemaBuilder = struct {
    allocator: std.mem.Allocator,
    description: []const u8,
    tag_position: u32,
    tables: std.ArrayListUnmanaged(TableDef),

    pub fn init(allocator: std.mem.Allocator) SchemaBuilder {
        return .{
            .allocator = allocator,
            .description = "",
            .tag_position = Schema.NO_TAG,
            .tables = .{},
        };
    }

    pub fn deinit(self: *SchemaBuilder) void {
        self.tables.deinit(self.allocator);
    }

    pub fn setDescription(self: *SchemaBuilder, desc: []const u8) *SchemaBuilder {
        self.description = desc;
        return self;
    }

    pub fn setTagPosition(self: *SchemaBuilder, pos: u32) *SchemaBuilder {
        self.tag_position = pos;
        return self;
    }

    pub fn addTable(self: *SchemaBuilder, table: TableDef) !*SchemaBuilder {
        try self.tables.append(self.allocator, table);
        return self;
    }

    pub fn build(self: *SchemaBuilder) !*Schema {
        const schema = try self.allocator.create(Schema);

        // Copy description
        const desc = if (self.description.len > 0)
            try self.allocator.dupe(u8, self.description)
        else
            "";

        schema.* = .{
            .version = Schema.CURRENT_VERSION,
            .description = desc,
            .tag_position = self.tag_position,
            .tables = try self.tables.toOwnedSlice(self.allocator),
            .flags = .{},
        };

        return schema;
    }
};

/// Builder for constructing table definitions
pub const TableBuilder = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    tag: u8,
    fields: std.ArrayListUnmanaged(FieldDef),
    is_default: bool,

    pub fn init(allocator: std.mem.Allocator, name: []const u8) TableBuilder {
        return .{
            .allocator = allocator,
            .name = name,
            .tag = 0,
            .fields = .{},
            .is_default = false,
        };
    }

    pub fn deinit(self: *TableBuilder) void {
        self.fields.deinit(self.allocator);
    }

    pub fn setTag(self: *TableBuilder, tag: u8) *TableBuilder {
        self.tag = tag;
        return self;
    }

    pub fn setDefault(self: *TableBuilder) *TableBuilder {
        self.is_default = true;
        return self;
    }

    pub fn addField(
        self: *TableBuilder,
        name: []const u8,
        field_type: FieldType,
        position: u32,
        length: u32,
    ) !*TableBuilder {
        const field_name = try self.allocator.dupe(u8, name);
        try self.fields.append(self.allocator, .{
            .name = field_name,
            .field_type = field_type,
            .position = position,
            .length = length,
            .decimal_places = 0,
            .flags = .{},
        });
        return self;
    }

    pub fn addDecimalField(
        self: *TableBuilder,
        name: []const u8,
        position: u32,
        length: u32,
        decimal_places: u8,
    ) !*TableBuilder {
        const field_name = try self.allocator.dupe(u8, name);
        try self.fields.append(self.allocator, .{
            .name = field_name,
            .field_type = .decimal,
            .position = position,
            .length = length,
            .decimal_places = decimal_places,
            .flags = .{},
        });
        return self;
    }

    pub fn build(self: *TableBuilder) !TableDef {
        // Calculate record size from fields
        var max_end: u32 = 0;
        for (self.fields.items) |field| {
            const end = field.position + field.length;
            if (end > max_end) max_end = end;
        }

        const table_name = try self.allocator.dupe(u8, self.name);

        return TableDef{
            .name = table_name,
            .tag = self.tag,
            .record_size = max_end,
            .fields = try self.fields.toOwnedSlice(self.allocator),
            .flags = .{ .is_default = self.is_default },
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "schema builder single table" {
    const allocator = std.testing.allocator;

    var table_builder = TableBuilder.init(allocator, "product");
    defer table_builder.deinit();

    _ = try table_builder.addField("prod_id", .alpha, 0, 10);
    _ = try table_builder.addField("name", .alpha, 10, 30);
    _ = try table_builder.addDecimalField("price", 40, 8, 2);

    const table = try table_builder.build();

    var schema_builder = SchemaBuilder.init(allocator);
    defer schema_builder.deinit();

    _ = schema_builder.setDescription("Product catalog");
    _ = try schema_builder.addTable(table);

    const schema = try schema_builder.build();
    defer schema.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), schema.tables.len);
    try std.testing.expectEqual(@as(u32, 48), schema.tables[0].record_size);
    try std.testing.expectEqual(@as(usize, 3), schema.tables[0].fields.len);
    try std.testing.expect(!schema.isMultiTable());
}

test "schema builder multi-table with tags" {
    const allocator = std.testing.allocator;

    // Header table (tag 'H')
    var header_builder = TableBuilder.init(allocator, "order_header");
    _ = header_builder.setTag('H');
    _ = header_builder.setDefault();
    _ = try header_builder.addField("tag", .alpha, 0, 1);
    _ = try header_builder.addField("order_id", .alpha, 1, 10);
    _ = try header_builder.addField("customer", .alpha, 11, 8);
    _ = try header_builder.addDecimalField("total", 19, 10, 2);
    const header_table = try header_builder.build();

    // Detail table (tag 'D')
    var detail_builder = TableBuilder.init(allocator, "order_detail");
    _ = detail_builder.setTag('D');
    _ = try detail_builder.addField("tag", .alpha, 0, 1);
    _ = try detail_builder.addField("order_id", .alpha, 1, 10);
    _ = try detail_builder.addField("product_id", .alpha, 11, 10);
    _ = try detail_builder.addDecimalField("quantity", 21, 6, 0);
    _ = try detail_builder.addDecimalField("price", 27, 8, 2);
    const detail_table = try detail_builder.build();

    var schema_builder = SchemaBuilder.init(allocator);
    defer schema_builder.deinit();

    _ = schema_builder.setDescription("Order management");
    _ = schema_builder.setTagPosition(0); // Tag at byte 0
    _ = try schema_builder.addTable(header_table);
    _ = try schema_builder.addTable(detail_table);

    const schema = try schema_builder.build();
    defer schema.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 2), schema.tables.len);
    try std.testing.expectEqual(@as(u32, 0), schema.tag_position);
    try std.testing.expect(schema.isMultiTable());

    // Lookup by tag
    const header = schema.getTableByTag('H');
    try std.testing.expect(header != null);
    try std.testing.expectEqualStrings("order_header", header.?.name);

    const detail = schema.getTableByTag('D');
    try std.testing.expect(detail != null);
    try std.testing.expectEqualStrings("order_detail", detail.?.name);
}

test "schema serialization roundtrip" {
    const allocator = std.testing.allocator;

    // Build a schema
    var table_builder = TableBuilder.init(allocator, "customer");
    _ = try table_builder.addField("cust_id", .alpha, 0, 8);
    _ = try table_builder.addField("name", .alpha, 8, 30);
    _ = try table_builder.addDecimalField("balance", 38, 10, 2);
    const table = try table_builder.build();

    var schema_builder = SchemaBuilder.init(allocator);
    _ = schema_builder.setDescription("Customer database");
    _ = try schema_builder.addTable(table);
    const schema = try schema_builder.build();
    defer schema.deinit(allocator);

    // Serialize
    var buffer: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    try schema.serialize(stream.writer());

    // Deserialize
    stream.reset();
    const restored = try Schema.deserialize(stream.reader(), allocator);
    defer restored.deinit(allocator);

    // Verify
    try std.testing.expectEqualStrings("Customer database", restored.description);
    try std.testing.expectEqual(@as(usize, 1), restored.tables.len);
    try std.testing.expectEqualStrings("customer", restored.tables[0].name);
    try std.testing.expectEqual(@as(usize, 3), restored.tables[0].fields.len);
    try std.testing.expectEqualStrings("cust_id", restored.tables[0].fields[0].name);
}
