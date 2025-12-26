//! Ziggy ISAM C ABI - C-compatible exports for .NET interop
//!
//! This module provides a C-compatible interface to Ziggy ISAM, enabling:
//! - .NET P/Invoke calls to native ISAM operations
//! - Cross-language interoperability via stable C ABI
//! - Schema introspection from managed code
//!
//! All exported functions use C calling convention and simple types
//! (pointers, integers, null-terminated strings) for maximum compatibility.

const std = @import("std");
const isam = @import("isam.zig");
const schema_mod = @import("schema.zig");

// Re-export core types for visibility
pub const IsamFile = isam.IsamFile;
pub const Schema = isam.Schema;
pub const TableDef = isam.TableDef;
pub const FieldDef = isam.FieldDef;
pub const RFA = isam.RFA;
pub const ULID = isam.ULID;

// ============================================================================
// Error Codes (C-compatible)
// ============================================================================

pub const ZiggyError = enum(i32) {
    ok = 0,
    file_not_found = -1,
    invalid_format = -2,
    corrupted_index = -3,
    key_not_found = -4,
    duplicate_key = -5,
    record_locked = -6,
    end_of_file = -7,
    invalid_key = -8,
    out_of_memory = -9,
    io_error = -10,
    invalid_ulid = -11,
    ulid_not_found = -12,
    invalid_handle = -13,
    invalid_argument = -14,
};

fn toZiggyError(err: isam.IsamError) ZiggyError {
    return switch (err) {
        error.FileNotFound => .file_not_found,
        error.InvalidFormat => .invalid_format,
        error.CorruptedIndex => .corrupted_index,
        error.KeyNotFound => .key_not_found,
        error.DuplicateKey => .duplicate_key,
        error.RecordLocked => .record_locked,
        error.EndOfFile => .end_of_file,
        error.InvalidKey => .invalid_key,
        error.OutOfMemory => .out_of_memory,
        error.IoError => .io_error,
        error.InvalidUlid => .invalid_ulid,
        error.UlidNotFound => .ulid_not_found,
    };
}

// ============================================================================
// Opaque Handle Types
// ============================================================================

/// Opaque handle to an ISAM file
pub const ZiggyHandle = *anyopaque;

/// Handle to a schema
pub const ZiggySchemaHandle = *anyopaque;

// ============================================================================
// Global State
// ============================================================================

/// We use a general purpose allocator for all C ABI operations
var gpa = std.heap.GeneralPurposeAllocator(.{}){};

fn getAllocator() std.mem.Allocator {
    return gpa.allocator();
}

// ============================================================================
// File Operations
// ============================================================================

/// Create a new ISAM file
/// filename: null-terminated path (without extension)
/// record_size: size of each record
/// key_start: start position of primary key
/// key_length: length of primary key
/// Returns: handle or null on error
export fn ziggy_create(
    filename: [*:0]const u8,
    record_size: u32,
    key_start: u32,
    key_length: u32,
) ?ZiggyHandle {
    const allocator = getAllocator();
    const name = std.mem.span(filename);

    const segments = allocator.alloc(isam.KeyDef.KeySegment, 1) catch return null;
    segments[0] = .{
        .start = key_start,
        .length = key_length,
        .key_type = .alpha,
    };

    const key_defs = [_]isam.KeyDef{
        .{
            .segments = segments,
            .allow_duplicates = false,
            .changes_allowed = false,
            .key_number = 0,
        },
    };

    const file = IsamFile.create(allocator, name, &key_defs, record_size, .{}) catch return null;
    return @ptrCast(file);
}

/// Create a new ISAM file with schema
export fn ziggy_create_with_schema(
    filename: [*:0]const u8,
    record_size: u32,
    key_start: u32,
    key_length: u32,
    schema_handle: ?ZiggySchemaHandle,
) ?ZiggyHandle {
    const allocator = getAllocator();
    const name = std.mem.span(filename);

    const segments = allocator.alloc(isam.KeyDef.KeySegment, 1) catch return null;
    segments[0] = .{
        .start = key_start,
        .length = key_length,
        .key_type = .alpha,
    };

    const key_defs = [_]isam.KeyDef{
        .{
            .segments = segments,
            .allow_duplicates = false,
            .changes_allowed = false,
            .key_number = 0,
        },
    };

    const schema: ?*Schema = if (schema_handle) |h|
        @ptrCast(@alignCast(h))
    else
        null;

    const file = IsamFile.create(allocator, name, &key_defs, record_size, .{ .schema = schema }) catch return null;
    return @ptrCast(file);
}

/// Open an existing ISAM file
/// filename: null-terminated path (without extension)
/// mode: 0=read-only, 1=read-write, 2=exclusive
/// Returns: handle or null on error
export fn ziggy_open(filename: [*:0]const u8, mode: i32) ?ZiggyHandle {
    const allocator = getAllocator();
    const name = std.mem.span(filename);

    const open_mode: IsamFile.OpenMode = switch (mode) {
        0 => .read_only,
        1 => .read_write,
        2 => .exclusive,
        else => .read_write,
    };

    const file = IsamFile.open(allocator, name, open_mode) catch return null;
    return @ptrCast(file);
}

/// Close an ISAM file
export fn ziggy_close(handle: ?ZiggyHandle) void {
    if (handle) |h| {
        const file: *IsamFile = @ptrCast(@alignCast(h));
        file.close();
    }
}

// ============================================================================
// Record Operations
// ============================================================================

/// Store a new record
/// Returns: error code (0 = success)
export fn ziggy_store(
    handle: ZiggyHandle,
    record: [*]const u8,
    record_len: u32,
) ZiggyError {
    const file: *IsamFile = @ptrCast(@alignCast(handle));
    _ = file.store(record[0..record_len]) catch |e| return toZiggyError(e);
    return .ok;
}

/// Store a new record and return its ULID
/// ulid_out: buffer to receive 26-byte ULID string
export fn ziggy_store_with_ulid(
    handle: ZiggyHandle,
    record: [*]const u8,
    record_len: u32,
    ulid_out: [*]u8,
) ZiggyError {
    const file: *IsamFile = @ptrCast(@alignCast(handle));
    const ulid = file.storeWithUlid(record[0..record_len]) catch |e| return toZiggyError(e);
    const encoded = ulid.encode();
    @memcpy(ulid_out[0..26], &encoded);
    return .ok;
}

/// Read a record by key
/// key: key value to search for
/// key_len: length of key
/// record_out: buffer to receive record data
/// record_size: size of output buffer
/// bytes_read: receives actual bytes read
export fn ziggy_read(
    handle: ZiggyHandle,
    key: [*]const u8,
    key_len: u32,
    record_out: [*]u8,
    record_size: u32,
    bytes_read: *u32,
) ZiggyError {
    const file: *IsamFile = @ptrCast(@alignCast(handle));
    const len = file.read(key[0..key_len], record_out[0..record_size], .{}) catch |e| return toZiggyError(e);
    bytes_read.* = @intCast(len);
    return .ok;
}

/// Read a record by key with match mode
/// match_mode: 0=exact, 1=greater_equal, 2=greater, 3=partial
export fn ziggy_read_with_mode(
    handle: ZiggyHandle,
    key: [*]const u8,
    key_len: u32,
    match_mode: i32,
    record_out: [*]u8,
    record_size: u32,
    bytes_read: *u32,
) ZiggyError {
    const file: *IsamFile = @ptrCast(@alignCast(handle));

    const mode: isam.MatchMode = switch (match_mode) {
        0 => .exact,
        1 => .greater_equal,
        2 => .greater,
        3 => .partial,
        else => .greater_equal,
    };

    const len = file.read(key[0..key_len], record_out[0..record_size], .{ .match_mode = mode }) catch |e| return toZiggyError(e);
    bytes_read.* = @intCast(len);
    return .ok;
}

/// Read next sequential record
export fn ziggy_read_next(
    handle: ZiggyHandle,
    record_out: [*]u8,
    record_size: u32,
    bytes_read: *u32,
) ZiggyError {
    const file: *IsamFile = @ptrCast(@alignCast(handle));
    const len = file.readNext(record_out[0..record_size]) catch |e| return toZiggyError(e);
    bytes_read.* = @intCast(len);
    return .ok;
}

/// Read a record by ULID string
export fn ziggy_read_by_ulid(
    handle: ZiggyHandle,
    ulid_str: [*]const u8,
    record_out: [*]u8,
    record_size: u32,
    bytes_read: *u32,
) ZiggyError {
    const file: *IsamFile = @ptrCast(@alignCast(handle));
    const len = file.readByUlidString(ulid_str[0..26], record_out[0..record_size], .{}) catch |e| return toZiggyError(e);
    bytes_read.* = @intCast(len);
    return .ok;
}

/// Update current record
export fn ziggy_write(
    handle: ZiggyHandle,
    record: [*]const u8,
    record_len: u32,
) ZiggyError {
    const file: *IsamFile = @ptrCast(@alignCast(handle));
    file.write(record[0..record_len]) catch |e| return toZiggyError(e);
    return .ok;
}

/// Delete current record
export fn ziggy_delete(handle: ZiggyHandle) ZiggyError {
    const file: *IsamFile = @ptrCast(@alignCast(handle));
    file.delete() catch |e| return toZiggyError(e);
    return .ok;
}

/// Unlock current record
export fn ziggy_unlock(handle: ZiggyHandle) void {
    const file: *IsamFile = @ptrCast(@alignCast(handle));
    file.unlock();
}

/// Flush buffers to disk
export fn ziggy_flush(handle: ZiggyHandle) ZiggyError {
    const file: *IsamFile = @ptrCast(@alignCast(handle));
    file.flush() catch |e| return toZiggyError(e);
    return .ok;
}

// ============================================================================
// Record Position / ULID Access
// ============================================================================

/// Get ULID of current record as 26-byte string
/// Returns: 1 if ULID available, 0 if not
export fn ziggy_get_current_ulid(handle: ZiggyHandle, ulid_out: [*]u8) i32 {
    const file: *IsamFile = @ptrCast(@alignCast(handle));
    if (file.getRecordUlidString()) |ulid| {
        @memcpy(ulid_out[0..26], &ulid);
        return 1;
    }
    return 0;
}

// ============================================================================
// Schema Operations
// ============================================================================

/// Get schema from open file
/// Returns: schema handle or null if no schema
export fn ziggy_get_schema(handle: ZiggyHandle) ?ZiggySchemaHandle {
    const file: *IsamFile = @ptrCast(@alignCast(handle));
    if (file.getSchema()) |s| {
        return @ptrCast(s);
    }
    return null;
}

/// Check if schema is multi-table
export fn ziggy_schema_is_multi_table(schema_handle: ZiggySchemaHandle) i32 {
    const schema: *Schema = @ptrCast(@alignCast(schema_handle));
    return if (schema.isMultiTable()) 1 else 0;
}

/// Get number of tables in schema
export fn ziggy_schema_table_count(schema_handle: ZiggySchemaHandle) u32 {
    const schema: *Schema = @ptrCast(@alignCast(schema_handle));
    return @intCast(schema.tables.len);
}

/// Get tag position in schema
export fn ziggy_schema_tag_position(schema_handle: ZiggySchemaHandle) u32 {
    const schema: *Schema = @ptrCast(@alignCast(schema_handle));
    return schema.tag_position;
}

/// Get schema description length
export fn ziggy_schema_description_len(schema_handle: ZiggySchemaHandle) u32 {
    const schema: *Schema = @ptrCast(@alignCast(schema_handle));
    return @intCast(schema.description.len);
}

/// Get schema description
export fn ziggy_schema_description(schema_handle: ZiggySchemaHandle, buf: [*]u8, buf_len: u32) u32 {
    const schema: *Schema = @ptrCast(@alignCast(schema_handle));
    const copy_len = @min(schema.description.len, buf_len);
    @memcpy(buf[0..copy_len], schema.description[0..copy_len]);
    return @intCast(copy_len);
}

// ============================================================================
// Table Operations
// ============================================================================

/// Opaque table handle
pub const ZiggyTableHandle = *const anyopaque;

/// Get table by index
export fn ziggy_schema_get_table(schema_handle: ZiggySchemaHandle, index: u32) ?ZiggyTableHandle {
    const schema: *Schema = @ptrCast(@alignCast(schema_handle));
    if (index >= schema.tables.len) return null;
    return @ptrCast(&schema.tables[index]);
}

/// Get table by tag byte
export fn ziggy_schema_get_table_by_tag(schema_handle: ZiggySchemaHandle, tag: u8) ?ZiggyTableHandle {
    const schema: *Schema = @ptrCast(@alignCast(schema_handle));
    if (schema.getTableByTag(tag)) |t| {
        return @ptrCast(t);
    }
    return null;
}

/// Get table name length
export fn ziggy_table_name_len(table_handle: ZiggyTableHandle) u32 {
    const table: *const TableDef = @ptrCast(@alignCast(table_handle));
    return @intCast(table.name.len);
}

/// Get table name
export fn ziggy_table_name(table_handle: ZiggyTableHandle, buf: [*]u8, buf_len: u32) u32 {
    const table: *const TableDef = @ptrCast(@alignCast(table_handle));
    const copy_len = @min(table.name.len, buf_len);
    @memcpy(buf[0..copy_len], table.name[0..copy_len]);
    return @intCast(copy_len);
}

/// Get table tag byte
export fn ziggy_table_tag(table_handle: ZiggyTableHandle) u8 {
    const table: *const TableDef = @ptrCast(@alignCast(table_handle));
    return table.tag;
}

/// Get table record size
export fn ziggy_table_record_size(table_handle: ZiggyTableHandle) u32 {
    const table: *const TableDef = @ptrCast(@alignCast(table_handle));
    return table.record_size;
}

/// Get number of fields in table
export fn ziggy_table_field_count(table_handle: ZiggyTableHandle) u32 {
    const table: *const TableDef = @ptrCast(@alignCast(table_handle));
    return @intCast(table.fields.len);
}

/// Check if table is default
export fn ziggy_table_is_default(table_handle: ZiggyTableHandle) i32 {
    const table: *const TableDef = @ptrCast(@alignCast(table_handle));
    return if (table.flags.is_default) 1 else 0;
}

// ============================================================================
// Field Operations
// ============================================================================

/// Opaque field handle
pub const ZiggyFieldHandle = *const anyopaque;

/// Get field by index
export fn ziggy_table_get_field(table_handle: ZiggyTableHandle, index: u32) ?ZiggyFieldHandle {
    const table: *const TableDef = @ptrCast(@alignCast(table_handle));
    if (index >= table.fields.len) return null;
    return @ptrCast(&table.fields[index]);
}

/// Get field name length
export fn ziggy_field_name_len(field_handle: ZiggyFieldHandle) u32 {
    const field: *const FieldDef = @ptrCast(@alignCast(field_handle));
    return @intCast(field.name.len);
}

/// Get field name
export fn ziggy_field_name(field_handle: ZiggyFieldHandle, buf: [*]u8, buf_len: u32) u32 {
    const field: *const FieldDef = @ptrCast(@alignCast(field_handle));
    const copy_len = @min(field.name.len, buf_len);
    @memcpy(buf[0..copy_len], field.name[0..copy_len]);
    return @intCast(copy_len);
}

/// Get field type (see FieldType enum values)
export fn ziggy_field_type(field_handle: ZiggyFieldHandle) u8 {
    const field: *const FieldDef = @ptrCast(@alignCast(field_handle));
    return @intFromEnum(field.field_type);
}

/// Get field position in record
export fn ziggy_field_position(field_handle: ZiggyFieldHandle) u32 {
    const field: *const FieldDef = @ptrCast(@alignCast(field_handle));
    return field.position;
}

/// Get field length in bytes
export fn ziggy_field_length(field_handle: ZiggyFieldHandle) u32 {
    const field: *const FieldDef = @ptrCast(@alignCast(field_handle));
    return field.length;
}

/// Get field decimal places
export fn ziggy_field_decimal_places(field_handle: ZiggyFieldHandle) u8 {
    const field: *const FieldDef = @ptrCast(@alignCast(field_handle));
    return field.decimal_places;
}

// ============================================================================
// Version / Info
// ============================================================================

/// Get library version string
export fn ziggy_version() [*:0]const u8 {
    return "0.1.0";
}

/// Get library name
export fn ziggy_name() [*:0]const u8 {
    return "Ziggy ISAM";
}

// ============================================================================
// Tests
// ============================================================================

test "cabi error conversion" {
    try std.testing.expectEqual(ZiggyError.ok, ZiggyError.ok);
    try std.testing.expectEqual(ZiggyError.file_not_found, toZiggyError(error.FileNotFound));
    try std.testing.expectEqual(ZiggyError.key_not_found, toZiggyError(error.KeyNotFound));
}

test "cabi version" {
    const ver = ziggy_version();
    try std.testing.expect(ver[0] == '0');
}
