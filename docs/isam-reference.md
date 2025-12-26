# ZiggyDB Reference

ZiggyDB is the ISAM (Indexed Sequential Access Method) database engine for Zibol, built on B+ tree indexing.

## Overview

ISAM files provide:
- Fast key-based record lookup
- Sequential access in key order
- Multiple indexes per file
- Fixed-length record storage

## File Structure

ZiggyDB uses a single-file database format (.zdb) for atomic operations and easy deployment:

| Extension | Contents |
|-----------|----------|
| .zdb | Header, key definitions, B+ tree index, record data |

### Database File Format (.zdb)

```
┌─────────────────────────────────────────┐
│ Header (256 bytes)                      │
│ - Magic, version, key count, etc.       │
├─────────────────────────────────────────┤
│ Key Definitions (variable)              │
├─────────────────────────────────────────┤
│ Index Region (B-tree entries)           │
├─────────────────────────────────────────┤
│ Data Region (ULID + records)            │
└─────────────────────────────────────────┘
```

### Header Layout

```
Offset  Size    Field
------  ----    -----
0       8       Magic "ZIGGYDB\0"
8       4       Version (2)
12      4       Page size (4096)
16      2       Key count
18      2       Flags
20      8       Record count
28      4       Record size
32      1       Record type (0=fixed)
33      3       Padding
36      8       Key defs offset
44      8       Key defs size
52      8       Index offset
60      8       Index size
68      8       Data offset
76      8       Free list head
84-255  ...     Reserved
```

### Record Storage

Each record is stored with a database-managed ULID:

```
[ULID 16 bytes][length 4 bytes][record data]
```

ULIDs (Universally Unique Lexicographically Sortable Identifiers) provide:
- Human-readable 26-character identifiers (Crockford Base32)
- Lexicographically sortable by creation time
- URL-safe characters
- Globally unique across systems

## Creating ISAM Files

Use `XCALL ISAMC` to create a new ISAM file:

```zbl
xcall ISAMC(filespec, record_size, num_keys, key_spec1, ...)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| filespec | string | Base filename (without extension) |
| record_size | integer | Total record size in bytes |
| num_keys | integer | Number of key definitions (1-255) |
| key_spec | string | Key specification (one per key) |

### Key Specification Syntax

```
"START=pos, LENGTH=len[, TYPE=type][, DUPS][, MODIFY]"
```

| Field | Required | Description |
|-------|----------|-------------|
| START | Yes | Position in record (1-based) |
| LENGTH | Yes | Key length in bytes |
| TYPE | No | Key type (default: ALPHA) |
| DUPS | No | Allow duplicate keys |
| MODIFY | No | Key can be modified |

### Key Types

| Type | Description | Comparison |
|------|-------------|------------|
| ALPHA | Case-sensitive characters | Byte-by-byte |
| NOCASE | Case-insensitive characters | Uppercase comparison |
| DECIMAL | ASCII numeric digits | Numeric value |
| INTEGER | Binary integer | Signed integer |

### Examples

**Single Key:**
```zbl
record product
    prod_id     ,a10       ; Position 1, length 10
    prod_name   ,a30
    price       ,d8.2
endrecord

proc
    ; Record size = 10 + 30 + 8 = 48 bytes
    xcall ISAMC("products", 48, 1, "START=1, LENGTH=10, TYPE=ALPHA")
end
```

**Multiple Keys:**
```zbl
record employee
    emp_id      ,a8        ; Key 1: Position 1, length 8
    dept_id     ,a4        ; Key 2: Position 9, length 4
    emp_name    ,a30
endrecord

proc
    ; Two keys defined
    xcall ISAMC("employees", 42, 2,
        "START=1, LENGTH=8, TYPE=ALPHA",
        "START=9, LENGTH=4, TYPE=ALPHA, DUPS")
end
```

**With Duplicates:**
```zbl
xcall ISAMC("orders", 100, 1, "START=1, LENGTH=8, TYPE=ALPHA, DUPS")
```

## Opening ISAM Files

```zbl
open(channel, mode, filename)
```

### ISAM Modes

| Mode | Description |
|------|-------------|
| "U:I" | Update (read/write) ISAM |
| "I:I" | Input (read-only) ISAM |

### Examples

```zbl
record
    ch          ,i4
endrecord

proc
    ; Open for read/write
    open(ch, "U:I", "products")

    ; ... perform operations ...

    close(ch)
end
```

## Record Operations

### STORE - Insert New Record

```zbl
store(channel, record)
```

Inserts a new record and updates all indexes.

```zbl
prod_id = "PROD00001"
prod_name = "Widget"
price = 19.99
store(ch, product)
```

**Errors:**
- Duplicate key (if duplicates not allowed)

### READ - Keyed Read

```zbl
read(channel, record, key)
```

Finds and reads a record by key value.

```zbl
read(ch, product, "PROD00001")
display(tt, "Found: ", prod_name)
```

**Match Modes:**

| Mode | Description |
|------|-------------|
| Exact | Key must match exactly |
| Greater/Equal | First key >= value |
| Greater | First key > value |
| Partial | Key starts with value |

### READS - Sequential Read

```zbl
reads(channel, record)
```

Reads the next record in key order.

```zbl
; Read first record
reads(ch, product)
display(tt, prod_id)

; Read second record
reads(ch, product)
display(tt, prod_id)

; Continue until end of file...
```

### WRITE - Update Record

```zbl
write(channel, record)
```

Updates the current record (after READ or READS).

```zbl
read(ch, product, "PROD00001")
price = 24.99
write(ch, product)
```

### DELETE - Remove Record

```zbl
delete(channel)
```

Deletes the current record.

```zbl
read(ch, product, "PROD00001")
delete(ch)
```

## B+ Tree Implementation

ZiggyDB uses a B+ tree for indexing:

### Configuration

| Parameter | Value |
|-----------|-------|
| Order | 128 |
| Max keys per node | 127 |
| Min keys per node | 63 |
| Block size | 4096 bytes |

### Features

- All data pointers in leaf nodes
- Leaf nodes linked for sequential access
- Self-balancing on insert/delete
- Efficient range queries

### Record Identifiers

Each record has two types of identifiers:

**ULID (Universally Unique Lexicographically Sortable Identifier)**
- 26-character human-readable string
- Automatically generated on STORE
- Accessible via GETRFA qualifier
- Read directly via RFA qualifier

**RFA (Record File Address)**
- Internal 64-bit address (48-bit block + 16-bit offset)
- Used internally for B-tree pointers
- This allows addressing up to 2^48 blocks for massive file capacity

**Example: Using ULIDs**
```zbl
record
    ulid    ,a26        ; Storage for record ULID
endrecord

; Store and get ULID
store(ch, product, GETRFA:ulid)
display(tt, "Record ULID: ", ulid)

; Later, read directly by ULID
read(ch, product, ulid, RFA)
```

## Sequential Access

ISAM files support efficient sequential access in key order:

```zbl
; Position at beginning (first READS)
reads(ch, record)

; Read in key order
reads(ch, record)
reads(ch, record)
reads(ch, record)
```

### Traversal

The B+ tree leaf nodes are linked, allowing O(1) traversal to the next record once positioned.

## Best Practices

### Key Design

1. **Short keys** - Smaller keys = more keys per node = faster access
2. **Unique keys** - Avoid duplicates when possible
3. **Leading bytes** - Put the most selective bytes first

### Record Layout

```zbl
record customer
    ; Put primary key first
    cust_id     ,a8        ; Position 1

    ; Then alternate keys
    phone       ,a10       ; Position 9

    ; Then other fields
    name        ,a30       ; Position 19
    address     ,a50       ; Position 49
endrecord
```

### Channel Management

```zbl
record
    ch          ,i4        ; Use variables, not literals
endrecord

proc
    open(ch, "U:I", "customers")
    ; ... operations ...
    close(ch)              ; Always close when done
end
```

## Error Handling

| Error | Cause |
|-------|-------|
| File not found | ISAM file doesn't exist |
| Key not found | READ with non-existent key |
| Duplicate key | STORE with existing key (no DUPS) |
| End of file | READS past last record |

## Example: Complete ISAM Application

```zbl
; customer_app.dbl - Customer database application

record customer
    cust_id     ,a8
    cust_name   ,a30
    balance     ,d10.2
endrecord

record
    tt          ,i4
    ch          ,i4
endrecord

proc
    ; Setup terminal
    open(tt, "O", "tt:")

    display(tt, "=== Customer Database ===")
    display(tt, "")

    ; Create database (48 byte records)
    xcall ISAMC("custdb", 48, 1, "START=1, LENGTH=8, TYPE=ALPHA")
    display(tt, "Created database")

    ; Open for operations
    open(ch, "U:I", "custdb")

    ; Add customers
    cust_id = "C0000001"
    cust_name = "Acme Corporation"
    balance = 15000.00
    store(ch, customer)
    display(tt, "Added: ", cust_id, " - ", cust_name)

    cust_id = "C0000002"
    cust_name = "Beta Industries"
    balance = 8500.50
    store(ch, customer)
    display(tt, "Added: ", cust_id, " - ", cust_name)

    cust_id = "C0000003"
    cust_name = "Gamma Systems"
    balance = 22000.00
    store(ch, customer)
    display(tt, "Added: ", cust_id, " - ", cust_name)

    display(tt, "")
    display(tt, "--- Customer List ---")

    ; Read all customers in key order
    reads(ch, customer)
    display(tt, cust_id, " | ", cust_name, " | $", balance)

    reads(ch, customer)
    display(tt, cust_id, " | ", cust_name, " | $", balance)

    reads(ch, customer)
    display(tt, cust_id, " | ", cust_name, " | $", balance)

    ; Clean up
    close(ch)
    display(tt, "")
    display(tt, "Database closed")
    close(tt)
end
```

Output:
```
=== Customer Database ===

Created database
Added: C0000001 - Acme Corporation
Added: C0000002 - Beta Industries
Added: C0000003 - Gamma Systems

--- Customer List ---
C0000001 | Acme Corporation     | $15000
C0000002 | Beta Industries      | $8500
C0000003 | Gamma Systems        | $22000

Database closed
```

## Embedded Schema

ZiggyDB supports embedding table and field definitions directly in the database file. This enables:

- Runtime introspection of database structure
- .NET interop with auto-generated record accessors
- Multi-table support with tag byte discrimination
- Schema versioning and evolution

### Schema Structure

```
Schema
├── description: string
├── tag_position: u32 (0xFFFFFFFF if single-table)
├── tables[]
│   ├── name: string
│   ├── tag: u8 (discriminator byte value)
│   ├── record_size: u32
│   ├── is_default: bool
│   └── fields[]
│       ├── name: string
│       ├── type: FieldType
│       ├── position: u32
│       ├── length: u32
│       └── decimal_places: u8
```

### Field Types

| Type | Description | Zibol Equivalent |
|------|-------------|----------------|
| `alpha` | Character string | `,a` |
| `decimal` | Implied decimal | `,d` |
| `integer` | Binary integer | `,i` |
| `packed_decimal` | Packed BCD | `,p` |
| `date` | Date (YYYYMMDD) | - |
| `time` | Time (HHMMSS) | - |
| `datetime` | Combined | - |
| `boolean` | Boolean (0/1) | - |
| `binary` | Raw bytes | - |

### Multi-Table Files

A single ISAM file can contain multiple record types distinguished by a tag byte:

```zbl
; Multi-table order file with tag at position 0
record order_header
    tag         ,a1        ; 'H' = header record
    order_id    ,a8
    customer    ,a8
    total       ,d10.2
endrecord

record order_detail
    tag         ,a1        ; 'D' = detail record
    order_id    ,a8
    product     ,a10
    quantity    ,d6
endrecord
```

Both record types share the same key structure (order_id at position 1) but have different field layouts. The tag byte at position 0 identifies which table the record belongs to.

## .NET Interop

ZiggyDB provides a native library with C-compatible exports for use from .NET via P/Invoke.

### Architecture

```
┌─────────────────────────────────────────────────┐
│             .NET Application                     │
│  (C#, F#, VB.NET, etc.)                         │
└─────────────────────┬───────────────────────────┘
                      │ P/Invoke
┌─────────────────────▼───────────────────────────┐
│              ZiggyIsam.cs                        │
│  Managed wrapper classes                        │
└─────────────────────┬───────────────────────────┘
                      │ C ABI
┌─────────────────────▼───────────────────────────┐
│         libziggy_isam.dylib/.so/.dll            │
│  38 exported C functions                        │
└─────────────────────────────────────────────────┘
```

### Building the Native Library

```bash
zig build
```

This creates `zig-out/lib/libziggy_isam.dylib` (or `.so`/`.dll`).

### C# Example

```csharp
using Ziggy.Isam;

// Open database and introspect schema
using var file = IsamFile.Open("orders");

if (file.Schema != null)
{
    Console.WriteLine($"Tables: {file.Schema.Tables.Length}");
    foreach (var table in file.Schema.Tables)
    {
        Console.WriteLine($"  {table.Name}: {table.Fields.Length} fields");
    }
}

// Read records
var record = file.Read("ORD00001");
string ulid = file.GetCurrentUlid();
Console.WriteLine($"ULID: {ulid}");
```

### Documentation

- [.NET Interop Guide](../dotnet/README.md) - Full .NET documentation
- [C ABI Reference](cabi-reference.md) - Native function reference

## Technical Details

### Memory Usage

- Each open ISAM file maintains an in-memory B+ tree
- Tree nodes are 4KB aligned
- Key data is copied to owned memory

### Persistence

- Index serialized on close
- Data written immediately on store/write
- Headers updated on close

### Concurrency

Current implementation:
- Single-process access
- File-level locking (planned)
- Record-level locking (planned)

---

*ZiggyDB Database Reference v0.1.0*
