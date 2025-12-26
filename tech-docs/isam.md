# Ziggy DBL ISAM Database

## Overview

ISAM (Indexed Sequential Access Method) provides keyed random access and sequential access to records. Ziggy implements a modern ISAM system with B+ tree indexes, inspired by SQLite's design while maintaining SynergyDE compatibility.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     Runtime Layer                                │
│    executeStore() executeRead() executeReads() etc.             │
└───────────────────────────┬─────────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────────┐
│                      IsamFile                                    │
│    open() store() read() readNext() write() delete()           │
└────────────────┬─────────────────────────────┬──────────────────┘
                 │                             │
┌────────────────▼────────────────┐ ┌──────────▼──────────────────┐
│        Index Manager            │ │       Data Manager          │
│        (btree.zig)              │ │       (records)             │
│                                 │ │                             │
│   ┌─────────────────────┐       │ │   ┌─────────────────────┐   │
│   │     B+ Tree         │       │ │   │   Record Storage    │   │
│   │   (order 128)       │       │ │   │   (append-only)     │   │
│   │                     │       │ │   │                     │   │
│   │  Key → RFA mapping  │       │ │   │   RFA → Record      │   │
│   └─────────────────────┘       │ │   └─────────────────────┘   │
└────────────────┬────────────────┘ └──────────┬──────────────────┘
                 │                             │
┌────────────────▼────────────────┐ ┌──────────▼──────────────────┐
│         .ism file               │ │         .is1 file           │
│         (index)                 │ │         (data)              │
└─────────────────────────────────┘ └─────────────────────────────┘
```

## File Formats

### Index File (.ism)

```
Offset  Size    Field
──────────────────────────────────
0       8       Magic "ZIGGYIDX"
8       4       Version (1)
12      4       Block size (4096)
16      2       Key count
18      2       Flags
20      8       Root block offset
28      8       Record count
36      4       Reserved
40      N       KeyDef array (serialized)
...     ...     B+ tree blocks
```

### Data File (.is1)

```
Offset  Size    Field
──────────────────────────────────
0       8       Magic "ZIGGYDAT"
8       4       Version (1)
12      1       Record type (0=fixed, 1=variable)
13      4       Record size (fixed-length)
17      8       Free list head (future)
25      8       Record count
33      4       Flags
37      ...     Record data blocks
```

## Key Definitions

Keys are defined using segments that specify which portions of the record form the key:

```zig
pub const KeyDef = struct {
    segments: []KeySegment,
    flags: KeyFlags,
    key_number: u8,
};

pub const KeySegment = struct {
    start: u16,    // Byte offset in record
    length: u16,   // Byte length
    seg_type: SegmentType,
};

pub const KeyFlags = packed struct(u8) {
    duplicates_allowed: bool = false,
    changes_allowed: bool = true,
    null_key: bool = false,
    descending: bool = false,
    _reserved: u4 = 0,
};
```

### Creating ISAM Files

Use `XCALL ISAMC` to create files with key definitions:

```dbl
; Create file with 48-byte records, 1 key
; Key: bytes 1-8 (8-byte alpha key starting at position 1)
xcall ISAMC("customers", 48, 1, "START=1, LENGTH=8, TYPE=ALPHA")
```

The key specification string format:
```
START=n     - Starting position (1-based)
LENGTH=n    - Key length in bytes
TYPE=type   - ALPHA, DECIMAL, INTEGER
DUPS        - Allow duplicate keys
NODUPS      - Disallow duplicates (default)
NOMODIFY    - Key cannot be changed after store
```

## B+ Tree Implementation

### Node Structure

```zig
const ORDER = 128;  // High order for fewer disk seeks

pub const Node = struct {
    is_leaf: bool,
    key_count: u16,
    keys: [ORDER - 1]Key,

    // For internal nodes: child pointers
    children: [ORDER]u64,

    // For leaf nodes: record pointers + sibling link
    records: [ORDER - 1]RecordPtr,
    next_leaf: ?u64,
};

pub const Key = struct {
    data: []const u8,
};

pub const RecordPtr = struct {
    rfa: u48,      // Record file address
    flags: u16,
};
```

### Operations

**Insert**:
1. Extract key from record using KeyDef segments
2. Find appropriate leaf node via tree traversal
3. Insert key-RFA pair in sorted order
4. Split node if full (propagate up)

**Search**:
1. Start at root node
2. Binary search keys in node
3. Follow appropriate child pointer (internal) or return RFA (leaf)

**Sequential Access**:
1. Find starting position in tree
2. Follow `next_leaf` pointers for forward traversal
3. Iterate keys within each leaf node

### Key Extraction

```zig
fn extractKey(self: *Self, record: []const u8, key_idx: u8) IsamError![]u8 {
    const key_def = self.key_defs[key_idx];

    var total_len: usize = 0;
    for (key_def.segments) |seg| {
        total_len += seg.length;
    }

    const key_buffer = try self.allocator.alloc(u8, total_len);

    var offset: usize = 0;
    for (key_def.segments) |seg| {
        const start = seg.start;
        const end = start + seg.length;
        @memcpy(key_buffer[offset..], record[start..end]);
        offset += seg.length;
    }

    return key_buffer;
}
```

## IsamFile API

### Opening Files

```zig
pub fn open(
    allocator: Allocator,
    filename: []const u8,
    mode: OpenMode,
) IsamError!*IsamFile {
    // Determine file paths
    const index_path = try std.fmt.allocPrint(allocator, "{s}.ism", .{filename});
    const data_path = try std.fmt.allocPrint(allocator, "{s}.is1", .{filename});

    // Open files
    const index_file = try std.fs.cwd().openFile(index_path, .{ .mode = .read_write });
    const data_file = try std.fs.cwd().openFile(data_path, .{ .mode = .read_write });

    // Read headers and key definitions
    try self.readHeaders();
    try self.readKeyDefs();
    try self.loadBTree();

    return self;
}
```

### Store Operation

```zig
pub fn store(self: *Self, record: []const u8) IsamError!u48 {
    // Append record to data file
    const rfa = try self.appendRecord(record);

    // Insert into each key's B+ tree
    for (self.key_defs, 0..) |key_def, i| {
        const key = try self.extractKey(record, i);
        defer self.allocator.free(key);

        try self.btrees[i].insert(key, rfa);
    }

    // Update headers
    try self.writeHeaders();

    return rfa;
}
```

### Read Operation

```zig
pub fn read(
    self: *Self,
    key_value: []const u8,
    key_num: u8,
    match_mode: MatchMode,
) IsamError![]u8 {
    // Search B+ tree for key
    const rfa = try self.btrees[key_num].search(key_value, match_mode);

    if (rfa) |r| {
        // Read record from data file
        self.current_rfa = r;
        return try self.readRecord(r);
    }

    return IsamError.KeyNotFound;
}
```

### Sequential Read (READS)

```zig
pub fn readNext(self: *Self, key_num: u8) IsamError![]u8 {
    // Get next RFA from B+ tree leaf traversal
    const next_rfa = try self.btrees[key_num].getNextRfa(self.current_rfa);

    if (next_rfa) |r| {
        self.current_rfa = r;
        return try self.readRecord(r);
    }

    return IsamError.EndOfFile;
}
```

## Runtime Integration

The runtime connects DBL I/O statements to ISAM operations:

```zig
fn executeOpen(self: *Self, stmt: OpenStatement) !void {
    const channel_num = try self.evaluateExpression(stmt.channel);
    const filename = try self.evaluateExpression(stmt.filename);

    if (std.mem.indexOf(u8, stmt.mode, ":I") != null) {
        // ISAM mode
        const isam_file = try IsamFile.open(self.allocator, filename, mode);
        self.channels[channel_num] = Channel{
            .isam_file = isam_file,
            .is_isam = true,
        };
    } else {
        // Sequential file mode
        // ...
    }
}

fn executeStore(self: *Self, stmt: StoreStatement) !void {
    const channel = self.channels[stmt.channel];
    const record = self.getRecordBuffer();

    if (channel.is_isam) {
        _ = try channel.isam_file.store(record);
    } else {
        // Sequential write
    }
}
```

## SynergyDE Compatibility

### Supported Features

| Feature | Status | Notes |
|---------|--------|-------|
| Multiple keys per file | Yes | Via KeyDef array |
| Composite keys | Yes | Multiple segments |
| Duplicate keys | Yes | Via KeyFlags |
| READS sequential access | Yes | B+ tree leaf traversal |
| Key-based READ | Yes | Exact and partial match |
| STORE new records | Yes | Appends to data file |
| WRITE updates | Yes | In-place update |
| DELETE records | Partial | Marks as deleted |
| XCALL ISAMC | Yes | File creation |
| Record locking | Planned | Structures defined |

### Semantic Differences

1. **RFA Format**: 48-bit vs SynergyDE's 6-byte format
2. **Block Size**: 4096 default vs variable in SynergyDE
3. **Key Order**: 128 vs implementation-specific
4. **File Extension**: .ism/.is1 vs .ism/no extension

## Error Handling

```zig
pub const IsamError = error{
    FileNotFound,
    IoError,
    InvalidHeader,
    InvalidKeyDef,
    KeyNotFound,
    DuplicateKey,
    RecordLocked,
    EndOfFile,
    OutOfMemory,
    CorruptIndex,
    InvalidRfa,
};
```

## Performance Considerations

1. **B+ Tree Order**: 128 keys per node minimizes disk seeks
2. **Caching**: Future enhancement for hot nodes
3. **Bulk Loading**: Consider sorted inserts for initial load
4. **Key Length**: Shorter keys = more keys per node
5. **Sequential Access**: Leaf links enable efficient scans

## Future Enhancements

1. **Record Locking**: Per-record locks with wait queues
2. **Free Space Management**: Reuse deleted record slots
3. **Compression**: LZ4 for record data
4. **Crash Recovery**: Write-ahead logging
5. **Multi-Process Access**: File-based locking
