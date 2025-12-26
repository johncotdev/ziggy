# Ziggy ISAM Database Reference

Ziggy DBL includes a complete ISAM (Indexed Sequential Access Method) database engine built on B+ tree indexing.

## Overview

ISAM files provide:
- Fast key-based record lookup
- Sequential access in key order
- Multiple indexes per file
- Fixed-length record storage

## File Structure

Each ISAM database consists of two files:

| File | Extension | Contents |
|------|-----------|----------|
| Index | .ism | B+ tree index structure |
| Data | .is1 | Record data storage |

### Index File Format (.ism)

```
Offset  Size    Field
------  ----    -----
0       8       Magic "ZIGGYIDX"
8       4       Version (1)
12      4       Block size (4096)
16      2       Key count
18      2       Flags
20      8       Root block offset
28      8       Record count
36      ...     Key definitions
4096    ...     B+ tree blocks
```

### Data File Format (.is1)

```
Offset  Size    Field
------  ----    -----
0       8       Magic "ZIGGYDAT"
8       4       Version (1)
12      1       Record type (0=fixed)
13      4       Record size
17      8       Free list head
25      8       Record count
33      ...     Record data
```

## Creating ISAM Files

Use `XCALL ISAMC` to create a new ISAM file:

```dbl
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
```dbl
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
```dbl
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
```dbl
xcall ISAMC("orders", 100, 1, "START=1, LENGTH=8, TYPE=ALPHA, DUPS")
```

## Opening ISAM Files

```dbl
open(channel, mode, filename)
```

### ISAM Modes

| Mode | Description |
|------|-------------|
| "U:I" | Update (read/write) ISAM |
| "I:I" | Input (read-only) ISAM |

### Examples

```dbl
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

```dbl
store(channel, record)
```

Inserts a new record and updates all indexes.

```dbl
prod_id = "PROD00001"
prod_name = "Widget"
price = 19.99
store(ch, product)
```

**Errors:**
- Duplicate key (if duplicates not allowed)

### READ - Keyed Read

```dbl
read(channel, record, key)
```

Finds and reads a record by key value.

```dbl
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

```dbl
reads(channel, record)
```

Reads the next record in key order.

```dbl
; Read first record
reads(ch, product)
display(tt, prod_id)

; Read second record
reads(ch, product)
display(tt, prod_id)

; Continue until end of file...
```

### WRITE - Update Record

```dbl
write(channel, record)
```

Updates the current record (after READ or READS).

```dbl
read(ch, product, "PROD00001")
price = 24.99
write(ch, product)
```

### DELETE - Remove Record

```dbl
delete(channel)
```

Deletes the current record.

```dbl
read(ch, product, "PROD00001")
delete(ch)
```

## B+ Tree Implementation

Ziggy ISAM uses a B+ tree for indexing:

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

### Record File Address (RFA)

Each record is identified by an RFA:

```
48-bit block number + 16-bit offset = 64-bit address
```

This allows addressing up to 2^48 blocks, each 4KB, for massive file capacity.

## Sequential Access

ISAM files support efficient sequential access in key order:

```dbl
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

```dbl
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

```dbl
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

```dbl
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

*Ziggy ISAM Database Reference v0.1.0*
