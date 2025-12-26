# ISAM Deep Dive

This document provides comprehensive details on Synergy ISAM files - the core of Synergy DBMS and the primary target for Ziggy DBL's custom implementation.

## What is ISAM?

**ISAM** = Indexed Sequential Access Method

ISAM provides:
- **Keyed access** - Fast retrieval by key value
- **Sequential access** - Ordered traversal
- **High performance** - Maintained as file grows
- **Multi-key support** - Multiple indexes on same data

## Physical File Structure

A Synergy ISAM file consists of two physical files:

```
customer.ism    ← Index file (keys, pointers)
customer.is1    ← Data file (actual records)
```

### Index File (.ism)

Hierarchical B-tree structure:

```
                    ┌─────────┐
                    │  Root   │  ← Read first on any keyed access
                    │  Block  │
                    └────┬────┘
                         │
           ┌─────────────┼─────────────┐
           │             │             │
      ┌────▼────┐   ┌────▼────┐   ┌────▼────┐
      │Separator│   │Separator│   │Separator│  ← Narrow key ranges
      │  Block  │   │  Block  │   │  Block  │
      └────┬────┘   └────┬────┘   └────┬────┘
           │             │             │
      ┌────▼────┐   ┌────▼────┐   ┌────▼────┐
      │  Leaf   │   │  Leaf   │   │  Leaf   │  ← Sorted keys + pointers
      │  Block  │   │  Block  │   │  Block  │
      └─────────┘   └─────────┘   └─────────┘
```

### Block Structure

- **Block** = Smallest unit that can be read/written
- **Block size** = Set at file creation (typically 512, 1024, 2048, 4096)
- Each keyed access requires one READ per tree level
- Large files (1M+ records) with long keys may need 5+ levels

### Data File (.is1)

Contains actual record data:
- Records stored contiguously or with gaps (deleted records)
- Record length can be fixed or variable
- RFA (Record File Address) points to physical location

## Record Types

### Fixed-Length Records
```
All records stored at same size regardless of actual data.

┌────────────────────────────────┐
│ Record 1 (100 bytes)           │
├────────────────────────────────┤
│ Record 2 (100 bytes)           │
├────────────────────────────────┤
│ Record 3 (100 bytes)           │
└────────────────────────────────┘
```

### Variable-Length Records
```
Record size adjustable via WRITE after STORE.

┌──────────────────┐
│ Rec 1 (50 bytes) │
├────────────────────────┐
│ Rec 2 (80 bytes)       │
├──────────────┐
│ Rec 3 (40 b) │
└──────────────┘
```

### Multiple Fixed-Length
Supports up to 32 different record lengths.

## Keys and Indexes

### Key Composition

A key consists of one or more fields or partial fields:

```dbl
structure customer
    cust_id         ,d8         ; Primary key
    company_name    ,a50        ; Alternate key
    state           ,a2         ;
    city            ,a30        ;
    ; state + city could be composite key
endstructure
```

### Key Specifications

```dbl
; Format: start_position:length:type
; Type: ALPHA, NOCASE, DECIMAL, INTEGER, etc.

; Simple key at position 1, 8 digits, decimal
key_spec = "1:8:DECIMAL"

; Composite key
key_spec = "59:2:ALPHA;61:30:ALPHA"  ; state + city
```

### Key Types

| Type | Description |
|------|-------------|
| `ALPHA` | Case-sensitive character key |
| `NOCASE` | Case-insensitive character key |
| `DECIMAL` | Numeric (ASCII digit) key |
| `INTEGER` | Binary integer key |
| `PACKED` | Packed decimal key |
| `DESCENDING` | Reverse sort order |

### Multiple Indexes

Single ISAM file can have multiple keys:

```dbl
; Key 0: Customer ID (primary)
; Key 1: Company Name
; Key 2: State + City

read(1, customer_rec, cust_id)                    ; Uses key 0
read(1, customer_rec, company, KEYNUM:1)          ; Uses key 1
read(1, customer_rec, search_key, KEYNUM:2)       ; Uses key 2
```

### Duplicate Keys

```dbl
; Allow duplicates for non-unique keys
key_spec = "1:50:ALPHA:DUPLICATES"

; CHANGES allow duplicate to change (re-sort on update)
key_spec = "1:50:ALPHA:DUPLICATES:CHANGES"
```

## File Operations

### Creating ISAM Files

```dbl
; ISAMC subroutine
record
    key_spec    ,a256
endrecord

proc
    ; Define key: position 1, length 8, decimal
    key_spec = "START=1, LENGTH=8, TYPE=DECIMAL"

    xcall isamc("customer.ism", 1, key_spec)
end
```

### Opening ISAM Files

```dbl
; Exclusive read/write access
open(1, U:I, "customer.ism", SHARE:Q_EXCL_RW)

; Shared access
open(1, U:I, "customer.ism", SHARE:Q_NO_EXCL)

; Read-only
open(1, I:I, "customer.ism")
```

### Reading Records

```dbl
; By key (exact match)
read(1, customer_rec, cust_id)

; By key (greater than or equal)
read(1, customer_rec, search_key, MATCH:Q_GEQ)

; By key (partial match)
read(1, customer_rec, partial_key, MATCH:Q_PARTIAL)

; Using alternate key
read(1, customer_rec, company_name, KEYNUM:1)

; Sequential read (current key order)
reads(1, customer_rec, eof_label)
```

### Match Modes

| Mode | Description |
|------|-------------|
| `Q_EQ` | Exact match only |
| `Q_GEQ` | Greater than or equal (default) |
| `Q_GTR` | Greater than |
| `Q_PARTIAL` | Partial key match |

### Writing Records

```dbl
; Add new record
store(1, customer_rec)

; Update current record (after READ)
write(1, customer_rec)

; Replace at specific key (caution!)
write(1, customer_rec, cust_id)
```

### Deleting Records

```dbl
; Delete current record
read(1, customer_rec, cust_id)
delete(1)
```

### Finding Position

```dbl
; Position without reading
find(1, , cust_id)

; Then read sequentially
reads(1, customer_rec, eof_label)
```

## Locking

### Lock Modes

| Mode | Description |
|------|-------------|
| `Q_NO_LOCK` | Don't lock record |
| `Q_AUTO_LOCK` | Auto lock on read, unlock on next read |
| `Q_MANUAL_LOCK` | Lock until explicit UNLOCK |

### Locking Pattern

```dbl
; Read and lock
read(1, customer_rec, cust_id, LOCK:Q_MANUAL_LOCK)

; Modify
customer_rec.balance = customer_rec.balance + amount

; Update
write(1, customer_rec)

; Release lock
unlock(1)
```

### Lock Waiting

```dbl
; Wait up to 10 seconds for lock
read(1, customer_rec, cust_id, WAIT:10, LOCK:Q_MANUAL_LOCK)
```

## Caching

### Cache Behavior by Share Mode

| Share Mode | Read Cache | Write Behavior |
|------------|------------|----------------|
| `Q_EXCL_RW` | Full caching | Deferred until CLOSE/FLUSH |
| `Q_EXCL_RO` | Cached | Write-through (immediate) |
| `Q_NO_EXCL` | Cached until update | Write-through |

### Flushing

```dbl
flush(1)        ; Force write cached data to disk
```

## RFA (Record File Address)

Direct record addressing without key lookup:

```dbl
record
    saved_rfa   ,a6
endrecord

proc
    ; Get RFA when reading
    read(1, customer_rec, cust_id, GETRFA:saved_rfa)

    ; Later, read directly by RFA
    read(1, customer_rec, RFA:saved_rfa)
end
```

## Information Subroutines

### ISINFO
```dbl
record
    rec_count   ,d10
    key_count   ,d2
endrecord

proc
    xcall isinfo(1, "NUMRECS", rec_count)
    xcall isinfo(1, "NUMKEYS", key_count)
end
```

### ISKEY
```dbl
record
    key_len     ,d3
    key_pos     ,d5
endrecord

proc
    ; Get info about key 0
    xcall iskey(1, 0, "KLENGTH", key_len)
    xcall iskey(1, 0, "KPOSITION", key_pos)
end
```

### ISSTS (Status)
```dbl
record
    file_status ,d1
endrecord

proc
    xcall issts(1, file_status)
    ; 0 = OK, 1 = needs verify, 2 = needs recovery
end
```

## Utilities

### bldism
Create ISAM file from parameters.

### isload
Load records into ISAM file.

### isutl
Verify, recover, and optimize ISAM files.

### fconvert
Convert between file types.

## Portable Storage

- ISAM format identical across Windows and Unix
- Files can be copied directly between platforms
- Exception: Files with native integers need conversion
- Use `I` option for portable integer storage

## Revision 6 ISAM

Latest format optimizations:
- 4K-aligned reads/writes for large sector drives
- Default page size: 4096 bytes
- Better performance on modern storage

## Ziggy DBL Implementation Considerations

For our custom ISAM in Zig, we need to implement:

1. **B-tree index structure**
   - Root, separator, and leaf blocks
   - Efficient key searching
   - Block splitting and merging

2. **Data file management**
   - Record storage and retrieval
   - Free space management
   - Variable-length record support

3. **Key management**
   - Multiple indexes
   - Composite keys
   - Duplicate key handling

4. **Locking**
   - Record-level locking
   - File-level locking
   - Lock waiting with timeout

5. **Caching**
   - Read caching
   - Write buffering
   - Cache invalidation

6. **Recovery**
   - Transaction logging
   - Crash recovery
   - Index verification/rebuild

7. **Portability**
   - Consistent byte order
   - Platform-independent format
