/*
 * Ziggy ISAM C API Header
 *
 * This header defines the C interface to the Ziggy ISAM library.
 * Use this for FFI bindings from other languages (Python, Rust, Go, etc.)
 * or for direct C/C++ integration.
 */

#ifndef ZIGGY_ISAM_H
#define ZIGGY_ISAM_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Error Codes
 * ============================================================================ */

typedef enum {
    ZIGGY_OK = 0,
    ZIGGY_FILE_NOT_FOUND = -1,
    ZIGGY_INVALID_FORMAT = -2,
    ZIGGY_CORRUPTED_INDEX = -3,
    ZIGGY_KEY_NOT_FOUND = -4,
    ZIGGY_DUPLICATE_KEY = -5,
    ZIGGY_RECORD_LOCKED = -6,
    ZIGGY_END_OF_FILE = -7,
    ZIGGY_INVALID_KEY = -8,
    ZIGGY_OUT_OF_MEMORY = -9,
    ZIGGY_IO_ERROR = -10,
    ZIGGY_INVALID_ULID = -11,
    ZIGGY_ULID_NOT_FOUND = -12,
    ZIGGY_INVALID_HANDLE = -13,
    ZIGGY_INVALID_ARGUMENT = -14
} ZiggyError;

/* ============================================================================
 * Field Types
 * ============================================================================ */

typedef enum {
    ZIGGY_FIELD_ALPHA = 0,
    ZIGGY_FIELD_DECIMAL = 1,
    ZIGGY_FIELD_INTEGER = 2,
    ZIGGY_FIELD_PACKED = 3,
    ZIGGY_FIELD_DATE = 10,
    ZIGGY_FIELD_TIME = 11,
    ZIGGY_FIELD_DATETIME = 12,
    ZIGGY_FIELD_BOOLEAN = 13,
    ZIGGY_FIELD_BINARY = 14
} ZiggyFieldType;

/* ============================================================================
 * Match Modes
 * ============================================================================ */

typedef enum {
    ZIGGY_MATCH_EXACT = 0,
    ZIGGY_MATCH_GREATER_EQUAL = 1,
    ZIGGY_MATCH_GREATER = 2,
    ZIGGY_MATCH_PARTIAL = 3
} ZiggyMatchMode;

/* ============================================================================
 * Open Modes
 * ============================================================================ */

typedef enum {
    ZIGGY_OPEN_READ_ONLY = 0,
    ZIGGY_OPEN_READ_WRITE = 1,
    ZIGGY_OPEN_EXCLUSIVE = 2
} ZiggyOpenMode;

/* ============================================================================
 * Opaque Handle Types
 * ============================================================================ */

typedef void* ZiggyHandle;
typedef void* ZiggySchemaHandle;
typedef const void* ZiggyTableHandle;
typedef const void* ZiggyFieldHandle;

/* ============================================================================
 * File Operations
 * ============================================================================ */

/**
 * Create a new ISAM file.
 *
 * @param filename Path to file (without .zdb extension)
 * @param record_size Size of each record in bytes
 * @param key_start Start position of primary key (0-based)
 * @param key_length Length of primary key
 * @return File handle, or NULL on error
 */
ZiggyHandle ziggy_create(
    const char* filename,
    uint32_t record_size,
    uint32_t key_start,
    uint32_t key_length);

/**
 * Create a new ISAM file with embedded schema.
 */
ZiggyHandle ziggy_create_with_schema(
    const char* filename,
    uint32_t record_size,
    uint32_t key_start,
    uint32_t key_length,
    ZiggySchemaHandle schema);

/**
 * Open an existing ISAM file.
 *
 * @param filename Path to file (without .zdb extension)
 * @param mode Open mode (0=read-only, 1=read-write, 2=exclusive)
 * @return File handle, or NULL on error
 */
ZiggyHandle ziggy_open(const char* filename, int32_t mode);

/**
 * Close an ISAM file.
 */
void ziggy_close(ZiggyHandle handle);

/* ============================================================================
 * Record Operations
 * ============================================================================ */

/**
 * Store a new record.
 */
ZiggyError ziggy_store(
    ZiggyHandle handle,
    const uint8_t* record,
    uint32_t record_len);

/**
 * Store a new record and return its ULID.
 *
 * @param ulid_out Buffer to receive 26-byte ULID string
 */
ZiggyError ziggy_store_with_ulid(
    ZiggyHandle handle,
    const uint8_t* record,
    uint32_t record_len,
    uint8_t* ulid_out);

/**
 * Read a record by key.
 *
 * @param key Key value to search for
 * @param key_len Length of key
 * @param record_out Buffer to receive record data
 * @param record_size Size of output buffer
 * @param bytes_read Receives actual bytes read
 */
ZiggyError ziggy_read(
    ZiggyHandle handle,
    const uint8_t* key,
    uint32_t key_len,
    uint8_t* record_out,
    uint32_t record_size,
    uint32_t* bytes_read);

/**
 * Read a record by key with match mode.
 */
ZiggyError ziggy_read_with_mode(
    ZiggyHandle handle,
    const uint8_t* key,
    uint32_t key_len,
    int32_t match_mode,
    uint8_t* record_out,
    uint32_t record_size,
    uint32_t* bytes_read);

/**
 * Read next sequential record.
 */
ZiggyError ziggy_read_next(
    ZiggyHandle handle,
    uint8_t* record_out,
    uint32_t record_size,
    uint32_t* bytes_read);

/**
 * Read a record by ULID string.
 *
 * @param ulid_str 26-byte ULID string
 */
ZiggyError ziggy_read_by_ulid(
    ZiggyHandle handle,
    const uint8_t* ulid_str,
    uint8_t* record_out,
    uint32_t record_size,
    uint32_t* bytes_read);

/**
 * Update current record.
 */
ZiggyError ziggy_write(
    ZiggyHandle handle,
    const uint8_t* record,
    uint32_t record_len);

/**
 * Delete current record.
 */
ZiggyError ziggy_delete(ZiggyHandle handle);

/**
 * Unlock current record.
 */
void ziggy_unlock(ZiggyHandle handle);

/**
 * Flush buffers to disk.
 */
ZiggyError ziggy_flush(ZiggyHandle handle);

/* ============================================================================
 * ULID Access
 * ============================================================================ */

/**
 * Get ULID of current record.
 *
 * @param ulid_out Buffer to receive 26-byte ULID string
 * @return 1 if ULID available, 0 if not
 */
int32_t ziggy_get_current_ulid(ZiggyHandle handle, uint8_t* ulid_out);

/* ============================================================================
 * Schema Operations
 * ============================================================================ */

/**
 * Get schema from open file.
 *
 * @return Schema handle, or NULL if no schema
 */
ZiggySchemaHandle ziggy_get_schema(ZiggyHandle handle);

int32_t ziggy_schema_is_multi_table(ZiggySchemaHandle schema);
uint32_t ziggy_schema_table_count(ZiggySchemaHandle schema);
uint32_t ziggy_schema_tag_position(ZiggySchemaHandle schema);
uint32_t ziggy_schema_description_len(ZiggySchemaHandle schema);
uint32_t ziggy_schema_description(ZiggySchemaHandle schema, uint8_t* buf, uint32_t buf_len);

/* ============================================================================
 * Table Operations
 * ============================================================================ */

ZiggyTableHandle ziggy_schema_get_table(ZiggySchemaHandle schema, uint32_t index);
ZiggyTableHandle ziggy_schema_get_table_by_tag(ZiggySchemaHandle schema, uint8_t tag);

uint32_t ziggy_table_name_len(ZiggyTableHandle table);
uint32_t ziggy_table_name(ZiggyTableHandle table, uint8_t* buf, uint32_t buf_len);
uint8_t ziggy_table_tag(ZiggyTableHandle table);
uint32_t ziggy_table_record_size(ZiggyTableHandle table);
uint32_t ziggy_table_field_count(ZiggyTableHandle table);
int32_t ziggy_table_is_default(ZiggyTableHandle table);

/* ============================================================================
 * Field Operations
 * ============================================================================ */

ZiggyFieldHandle ziggy_table_get_field(ZiggyTableHandle table, uint32_t index);

uint32_t ziggy_field_name_len(ZiggyFieldHandle field);
uint32_t ziggy_field_name(ZiggyFieldHandle field, uint8_t* buf, uint32_t buf_len);
uint8_t ziggy_field_type(ZiggyFieldHandle field);
uint32_t ziggy_field_position(ZiggyFieldHandle field);
uint32_t ziggy_field_length(ZiggyFieldHandle field);
uint8_t ziggy_field_decimal_places(ZiggyFieldHandle field);

/* ============================================================================
 * Version Info
 * ============================================================================ */

const char* ziggy_version(void);
const char* ziggy_name(void);

#ifdef __cplusplus
}
#endif

#endif /* ZIGGY_ISAM_H */
