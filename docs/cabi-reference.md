# ZiggyDB C ABI Reference

This document describes the C-compatible API exported by the ZiggyDB native library. Use this API for:

- .NET P/Invoke integration
- Python ctypes/cffi bindings
- Rust FFI
- Go cgo
- Any language with C FFI support

## Building the Library

```bash
cd /path/to/ziggy
zig build
```

Output files:
- `zig-out/lib/libziggy_isam.dylib` (macOS)
- `zig-out/lib/libziggy_isam.so` (Linux)
- `zig-out/lib/ziggy_isam.dll` (Windows)

## Header File

A C header is provided at `dotnet/ziggy_isam.h`. Include it in your C/C++ projects:

```c
#include "ziggy_isam.h"
```

## Error Codes

All functions that can fail return a `ZiggyError` code:

| Code | Name | Description |
|------|------|-------------|
| 0 | `ZIGGY_OK` | Success |
| -1 | `ZIGGY_FILE_NOT_FOUND` | File does not exist |
| -2 | `ZIGGY_INVALID_FORMAT` | File format is invalid |
| -3 | `ZIGGY_CORRUPTED_INDEX` | Index structure is corrupted |
| -4 | `ZIGGY_KEY_NOT_FOUND` | Key not found in index |
| -5 | `ZIGGY_DUPLICATE_KEY` | Duplicate key (when not allowed) |
| -6 | `ZIGGY_RECORD_LOCKED` | Record is locked by another process |
| -7 | `ZIGGY_END_OF_FILE` | End of file reached |
| -8 | `ZIGGY_INVALID_KEY` | Invalid key specification |
| -9 | `ZIGGY_OUT_OF_MEMORY` | Memory allocation failed |
| -10 | `ZIGGY_IO_ERROR` | I/O operation failed |
| -11 | `ZIGGY_INVALID_ULID` | Invalid ULID format |
| -12 | `ZIGGY_ULID_NOT_FOUND` | ULID not found in index |
| -13 | `ZIGGY_INVALID_HANDLE` | Invalid file handle |
| -14 | `ZIGGY_INVALID_ARGUMENT` | Invalid argument |

## Handle Types

The API uses opaque handle pointers:

| Type | Description |
|------|-------------|
| `ZiggyHandle` | ISAM file handle (`void*`) |
| `ZiggySchemaHandle` | Schema handle (`void*`) |
| `ZiggyTableHandle` | Table definition handle (`const void*`) |
| `ZiggyFieldHandle` | Field definition handle (`const void*`) |

## File Operations

### ziggy_create

Create a new ISAM database file.

```c
ZiggyHandle ziggy_create(
    const char* filename,
    uint32_t record_size,
    uint32_t key_start,
    uint32_t key_length
);
```

**Parameters:**
- `filename` - Path to file without extension (`.zdb` added automatically)
- `record_size` - Size of each record in bytes
- `key_start` - Start position of primary key (0-based)
- `key_length` - Length of primary key in bytes

**Returns:** File handle, or `NULL` on error.

**Example:**
```c
ZiggyHandle file = ziggy_create("customers", 48, 0, 8);
if (file == NULL) {
    // Handle error
}
```

### ziggy_create_with_schema

Create a new ISAM file with embedded schema metadata.

```c
ZiggyHandle ziggy_create_with_schema(
    const char* filename,
    uint32_t record_size,
    uint32_t key_start,
    uint32_t key_length,
    ZiggySchemaHandle schema
);
```

**Parameters:**
- `filename` - Path to file without extension
- `record_size` - Size of each record in bytes
- `key_start` - Start position of primary key (0-based)
- `key_length` - Length of primary key in bytes
- `schema` - Schema handle (or `NULL` for no schema)

**Returns:** File handle, or `NULL` on error.

### ziggy_open

Open an existing ISAM database file.

```c
ZiggyHandle ziggy_open(const char* filename, int32_t mode);
```

**Parameters:**
- `filename` - Path to file without extension
- `mode` - Open mode:
  - `0` = Read-only
  - `1` = Read-write
  - `2` = Exclusive

**Returns:** File handle, or `NULL` on error.

**Example:**
```c
ZiggyHandle file = ziggy_open("customers", 1);  // Read-write
if (file == NULL) {
    // File not found or error
}
```

### ziggy_close

Close an ISAM file and free resources.

```c
void ziggy_close(ZiggyHandle handle);
```

**Parameters:**
- `handle` - File handle (can be `NULL`, which is a no-op)

**Notes:**
- Always close files when done to ensure data is flushed
- Safe to call with `NULL`

## Record Operations

### ziggy_store

Store a new record in the database.

```c
ZiggyError ziggy_store(
    ZiggyHandle handle,
    const uint8_t* record,
    uint32_t record_len
);
```

**Parameters:**
- `handle` - File handle
- `record` - Record data buffer
- `record_len` - Length of record data

**Returns:** `ZIGGY_OK` on success, error code on failure.

**Example:**
```c
const char* record = "CUST0001John Smith              ";
ZiggyError err = ziggy_store(file, (uint8_t*)record, 48);
if (err != ZIGGY_OK) {
    // Handle error
}
```

### ziggy_store_with_ulid

Store a new record and retrieve its auto-generated ULID.

```c
ZiggyError ziggy_store_with_ulid(
    ZiggyHandle handle,
    const uint8_t* record,
    uint32_t record_len,
    uint8_t* ulid_out
);
```

**Parameters:**
- `handle` - File handle
- `record` - Record data buffer
- `record_len` - Length of record data
- `ulid_out` - Buffer to receive 26-byte ULID string (not null-terminated)

**Returns:** `ZIGGY_OK` on success, error code on failure.

**Example:**
```c
uint8_t ulid[26];
ZiggyError err = ziggy_store_with_ulid(file, record, 48, ulid);
if (err == ZIGGY_OK) {
    printf("Stored with ULID: %.26s\n", ulid);
}
```

### ziggy_read

Read a record by key with exact match.

```c
ZiggyError ziggy_read(
    ZiggyHandle handle,
    const uint8_t* key,
    uint32_t key_len,
    uint8_t* record_out,
    uint32_t record_size,
    uint32_t* bytes_read
);
```

**Parameters:**
- `handle` - File handle
- `key` - Key value to search for
- `key_len` - Length of key
- `record_out` - Buffer to receive record data
- `record_size` - Size of output buffer
- `bytes_read` - Receives actual bytes read

**Returns:** `ZIGGY_OK` on success, `ZIGGY_KEY_NOT_FOUND` if not found.

### ziggy_read_with_mode

Read a record by key with specified match mode.

```c
ZiggyError ziggy_read_with_mode(
    ZiggyHandle handle,
    const uint8_t* key,
    uint32_t key_len,
    int32_t match_mode,
    uint8_t* record_out,
    uint32_t record_size,
    uint32_t* bytes_read
);
```

**Parameters:**
- `handle` - File handle
- `key` - Key value to search for
- `key_len` - Length of key
- `match_mode` - Match mode:
  - `0` = Exact match
  - `1` = Greater or equal
  - `2` = Greater than
  - `3` = Partial match
- `record_out` - Buffer to receive record data
- `record_size` - Size of output buffer
- `bytes_read` - Receives actual bytes read

**Returns:** `ZIGGY_OK` on success, `ZIGGY_KEY_NOT_FOUND` if not found.

### ziggy_read_next

Read the next sequential record in key order.

```c
ZiggyError ziggy_read_next(
    ZiggyHandle handle,
    uint8_t* record_out,
    uint32_t record_size,
    uint32_t* bytes_read
);
```

**Parameters:**
- `handle` - File handle
- `record_out` - Buffer to receive record data
- `record_size` - Size of output buffer
- `bytes_read` - Receives actual bytes read

**Returns:** `ZIGGY_OK` on success, `ZIGGY_END_OF_FILE` at end of file.

**Example:**
```c
// Read all records sequentially
uint8_t record[48];
uint32_t len;

// Position at first record
ziggy_read_with_mode(file, "A", 1, 1, record, 48, &len);  // GEQ "A"

while (ziggy_read_next(file, record, 48, &len) == ZIGGY_OK) {
    // Process record
}
```

### ziggy_read_by_ulid

Read a record directly by its ULID.

```c
ZiggyError ziggy_read_by_ulid(
    ZiggyHandle handle,
    const uint8_t* ulid_str,
    uint8_t* record_out,
    uint32_t record_size,
    uint32_t* bytes_read
);
```

**Parameters:**
- `handle` - File handle
- `ulid_str` - 26-byte ULID string
- `record_out` - Buffer to receive record data
- `record_size` - Size of output buffer
- `bytes_read` - Receives actual bytes read

**Returns:** `ZIGGY_OK` on success, `ZIGGY_ULID_NOT_FOUND` if not found.

### ziggy_write

Update the current record (after a read operation).

```c
ZiggyError ziggy_write(
    ZiggyHandle handle,
    const uint8_t* record,
    uint32_t record_len
);
```

**Parameters:**
- `handle` - File handle
- `record` - Updated record data
- `record_len` - Length of record data

**Returns:** `ZIGGY_OK` on success, error code on failure.

**Notes:**
- Must have a current record (from prior read)
- The record's ULID is preserved

### ziggy_delete

Delete the current record.

```c
ZiggyError ziggy_delete(ZiggyHandle handle);
```

**Parameters:**
- `handle` - File handle

**Returns:** `ZIGGY_OK` on success, error code on failure.

### ziggy_unlock

Release the lock on the current record.

```c
void ziggy_unlock(ZiggyHandle handle);
```

**Parameters:**
- `handle` - File handle

### ziggy_flush

Flush all buffers to disk.

```c
ZiggyError ziggy_flush(ZiggyHandle handle);
```

**Parameters:**
- `handle` - File handle

**Returns:** `ZIGGY_OK` on success, error code on failure.

## ULID Access

### ziggy_get_current_ulid

Get the ULID of the current record.

```c
int32_t ziggy_get_current_ulid(ZiggyHandle handle, uint8_t* ulid_out);
```

**Parameters:**
- `handle` - File handle
- `ulid_out` - Buffer to receive 26-byte ULID string

**Returns:** `1` if ULID available, `0` if no current record.

## Schema Operations

### ziggy_get_schema

Get the embedded schema from an open file.

```c
ZiggySchemaHandle ziggy_get_schema(ZiggyHandle handle);
```

**Parameters:**
- `handle` - File handle

**Returns:** Schema handle, or `NULL` if no schema embedded.

### ziggy_schema_is_multi_table

Check if schema has multiple tables.

```c
int32_t ziggy_schema_is_multi_table(ZiggySchemaHandle schema);
```

**Returns:** `1` if multi-table, `0` if single-table.

### ziggy_schema_table_count

Get number of tables in schema.

```c
uint32_t ziggy_schema_table_count(ZiggySchemaHandle schema);
```

### ziggy_schema_tag_position

Get tag byte position for multi-table files.

```c
uint32_t ziggy_schema_tag_position(ZiggySchemaHandle schema);
```

**Returns:** Position of tag byte, or `0xFFFFFFFF` if single-table.

### ziggy_schema_description_len / ziggy_schema_description

Get schema description.

```c
uint32_t ziggy_schema_description_len(ZiggySchemaHandle schema);
uint32_t ziggy_schema_description(ZiggySchemaHandle schema, uint8_t* buf, uint32_t buf_len);
```

## Table Operations

### ziggy_schema_get_table

Get table by index.

```c
ZiggyTableHandle ziggy_schema_get_table(ZiggySchemaHandle schema, uint32_t index);
```

**Returns:** Table handle, or `NULL` if index out of range.

### ziggy_schema_get_table_by_tag

Get table by tag byte value.

```c
ZiggyTableHandle ziggy_schema_get_table_by_tag(ZiggySchemaHandle schema, uint8_t tag);
```

**Returns:** Table handle, or `NULL` if not found.

### Table Properties

```c
uint32_t ziggy_table_name_len(ZiggyTableHandle table);
uint32_t ziggy_table_name(ZiggyTableHandle table, uint8_t* buf, uint32_t buf_len);
uint8_t ziggy_table_tag(ZiggyTableHandle table);
uint32_t ziggy_table_record_size(ZiggyTableHandle table);
uint32_t ziggy_table_field_count(ZiggyTableHandle table);
int32_t ziggy_table_is_default(ZiggyTableHandle table);
```

## Field Operations

### ziggy_table_get_field

Get field by index.

```c
ZiggyFieldHandle ziggy_table_get_field(ZiggyTableHandle table, uint32_t index);
```

**Returns:** Field handle, or `NULL` if index out of range.

### Field Properties

```c
uint32_t ziggy_field_name_len(ZiggyFieldHandle field);
uint32_t ziggy_field_name(ZiggyFieldHandle field, uint8_t* buf, uint32_t buf_len);
uint8_t ziggy_field_type(ZiggyFieldHandle field);
uint32_t ziggy_field_position(ZiggyFieldHandle field);
uint32_t ziggy_field_length(ZiggyFieldHandle field);
uint8_t ziggy_field_decimal_places(ZiggyFieldHandle field);
```

### Field Types

| Value | Name | Description |
|-------|------|-------------|
| 0 | `ALPHA` | Character string |
| 1 | `DECIMAL` | Implied decimal |
| 2 | `INTEGER` | Binary integer |
| 3 | `PACKED` | Packed decimal (BCD) |
| 10 | `DATE` | Date (YYYYMMDD) |
| 11 | `TIME` | Time (HHMMSS) |
| 12 | `DATETIME` | Combined date/time |
| 13 | `BOOLEAN` | Boolean (0/1) |
| 14 | `BINARY` | Raw bytes |

## Version Information

### ziggy_version

Get library version string.

```c
const char* ziggy_version(void);
```

**Returns:** Null-terminated version string (e.g., "0.1.0").

### ziggy_name

Get library name.

```c
const char* ziggy_name(void);
```

**Returns:** Null-terminated name string ("ZiggyDB").

## Thread Safety

The ZiggyDB C API is **not** thread-safe. Each file handle should only be accessed from one thread at a time. For concurrent access, use one handle per thread or implement external synchronization.

## Memory Management

- All handles returned by the API are owned by the library
- Do not attempt to free handles directly
- Call `ziggy_close()` to release file handles
- Schema, table, and field handles are valid only while the file is open

## Complete Example

```c
#include <stdio.h>
#include <string.h>
#include "ziggy_isam.h"

int main() {
    // Create file
    ZiggyHandle file = ziggy_create("example", 48, 0, 8);
    if (!file) {
        fprintf(stderr, "Failed to create file\n");
        return 1;
    }

    // Store records
    const char* records[] = {
        "CUST0001Acme Corporation         0000015000",
        "CUST0002Beta Industries          0000008500",
        "CUST0003Gamma Systems            0000022000"
    };

    for (int i = 0; i < 3; i++) {
        uint8_t ulid[26];
        ZiggyError err = ziggy_store_with_ulid(file, (uint8_t*)records[i], 48, ulid);
        if (err == ZIGGY_OK) {
            printf("Stored: %.8s with ULID: %.26s\n", records[i], ulid);
        }
    }

    // Read by key
    uint8_t record[48];
    uint32_t len;
    ZiggyError err = ziggy_read(file, (uint8_t*)"CUST0002", 8, record, 48, &len);
    if (err == ZIGGY_OK) {
        printf("Found: %.*s\n", (int)len, record);
    }

    // Clean up
    ziggy_close(file);
    return 0;
}
```

## See Also

- [ISAM Reference](isam-reference.md) - Full ISAM documentation
- [.NET Interop](../dotnet/README.md) - .NET bindings documentation
