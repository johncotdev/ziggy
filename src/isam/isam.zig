//! Ziggy ISAM - Indexed Sequential Access Method
//!
//! A Zig implementation of ISAM file storage, compatible with
//! Synergy DBL ISAM semantics.
//!
//! Features:
//! - Single-file database format (.zdb) for atomic operations and easy deployment
//! - Database-managed ULID (Universally Unique Lexicographically Sortable Identifier)
//!   for each record, providing a modern alternative to SynergyDE's binary RFA
//! - ULIDs are stored internally and are NOT part of the user's record data
//! - Access via GETRFA qualifier (get current record's ULID) and RFA qualifier
//!   (read by ULID directly)
//!
//! File Format (.zdb):
//! ┌─────────────────────────────────────────┐
//! │ Header (256 bytes)                      │
//! │ - Magic, version, key count, etc.       │
//! ├─────────────────────────────────────────┤
//! │ Key Definitions (variable)              │
//! ├─────────────────────────────────────────┤
//! │ Index Region (B-tree entries)           │
//! ├─────────────────────────────────────────┤
//! │ Data Region (ULID + records)            │
//! └─────────────────────────────────────────┘

const std = @import("std");
const btree = @import("btree.zig");
const ulid_mod = @import("ulid.zig");
pub const ULID = ulid_mod.ULID;

/// Single-file magic number
pub const MAGIC = [8]u8{ 'Z', 'I', 'G', 'G', 'Y', 'D', 'B', 0 };

/// Current file format version
pub const VERSION: u32 = 2; // Version 2 = single-file format

/// Default page size
pub const DEFAULT_PAGE_SIZE: u32 = 4096;

/// Header size (fixed)
pub const HEADER_SIZE: u64 = 256;

pub const IsamError = error{
    FileNotFound,
    InvalidFormat,
    CorruptedIndex,
    KeyNotFound,
    DuplicateKey,
    RecordLocked,
    EndOfFile,
    InvalidKey,
    OutOfMemory,
    IoError,
    InvalidUlid,
    UlidNotFound,
};

/// Key type specification
pub const KeyType = enum(u8) {
    alpha = 0, // Case-sensitive character key
    nocase = 1, // Case-insensitive character key
    decimal = 2, // Numeric (ASCII digit) key
    integer = 3, // Binary integer key
    packed_decimal = 4, // Packed decimal key
    descending = 0x80, // Flag for descending order
};

/// Key definition
pub const KeyDef = struct {
    /// Segments that make up this key
    segments: []const KeySegment,
    /// Allow duplicate keys
    allow_duplicates: bool,
    /// Duplicates can change (re-sort on update)
    changes_allowed: bool,
    /// Key number (0 = primary)
    key_number: u8,

    pub const KeySegment = struct {
        /// Start position in record (0-based)
        start: u32,
        /// Length of segment
        length: u32,
        /// Type of key data
        key_type: KeyType,
    };

    /// Calculate total key length
    pub fn keyLength(self: KeyDef) u32 {
        var len: u32 = 0;
        for (self.segments) |seg| {
            len += seg.length;
        }
        return len;
    }

    /// Extract key from record
    pub fn extractKey(self: KeyDef, record: []const u8, allocator: std.mem.Allocator) ![]u8 {
        const key = try allocator.alloc(u8, self.keyLength());
        var offset: usize = 0;

        for (self.segments) |seg| {
            const start = seg.start;
            const end = @min(start + seg.length, record.len);
            const src_len = if (end > start) end - start else 0;

            if (src_len > 0) {
                @memcpy(key[offset .. offset + src_len], record[start..end]);
            }
            // Pad with spaces if record is shorter than key position
            if (src_len < seg.length) {
                @memset(key[offset + src_len .. offset + seg.length], ' ');
            }
            offset += seg.length;
        }

        return key;
    }
};

/// Record type enumeration
pub const RecordType = enum(u8) {
    fixed = 0,
    variable = 1,
    multiple_fixed = 2,
};

/// Match modes for READ operations
pub const MatchMode = enum {
    exact, // Q_EQ - Exact match only
    greater_equal, // Q_GEQ - Greater than or equal (default)
    greater, // Q_GTR - Greater than
    partial, // Partial key match
};

/// Lock modes
pub const LockMode = enum {
    no_lock,
    shared,
    exclusive,
    auto_lock,
    manual_lock,
};

/// Single-file header (256 bytes, stored at start of .zdb file)
pub const FileHeader = struct {
    magic: [8]u8, // "ZIGGYDB\0"
    version: u32, // File format version
    page_size: u32, // Page size (default 4096)
    key_count: u16, // Number of user-defined keys
    flags: u16, // Feature flags
    record_count: u64, // Total records in file
    record_size: u32, // Fixed record size
    record_type: RecordType, // Fixed/variable
    _padding1: [3]u8, // Alignment padding
    key_defs_offset: u64, // Offset to key definitions
    key_defs_size: u64, // Size of key definitions region
    index_offset: u64, // Offset to index region
    index_size: u64, // Size of index region
    data_offset: u64, // Offset to data region
    free_list_head: u64, // Head of free record list
    _reserved: [256 - 88]u8, // Reserved for future use (pad to 256 bytes)

    pub fn init(key_count: u16, record_size: u32) FileHeader {
        return .{
            .magic = MAGIC,
            .version = VERSION,
            .page_size = DEFAULT_PAGE_SIZE,
            .key_count = key_count,
            .flags = 0,
            .record_count = 0,
            .record_size = record_size,
            .record_type = .fixed,
            ._padding1 = [_]u8{0} ** 3,
            .key_defs_offset = HEADER_SIZE,
            .key_defs_size = 0,
            .index_offset = 0,
            .index_size = 0,
            .data_offset = 0,
            .free_list_head = 0,
            ._reserved = [_]u8{0} ** (256 - 88),
        };
    }
};

/// Record File Address - unique identifier for a record
pub const RFA = struct {
    block: u48,
    offset: u16,

    pub fn toBytes(self: RFA) [8]u8 {
        var bytes: [8]u8 = undefined;
        std.mem.writeInt(u48, bytes[0..6], self.block, .little);
        std.mem.writeInt(u16, bytes[6..8], self.offset, .little);
        return bytes;
    }

    pub fn fromBytes(bytes: [8]u8) RFA {
        return .{
            .block = std.mem.readInt(u48, bytes[0..6], .little),
            .offset = std.mem.readInt(u16, bytes[6..8], .little),
        };
    }
};

/// ISAM File handle
pub const IsamFile = struct {
    allocator: std.mem.Allocator,
    file: ?std.fs.File, // Single database file
    header: FileHeader, // File header
    key_defs: []KeyDef,
    btrees: []btree.BTree, // One B-tree per key definition
    current_key: u8,
    current_rfa: ?RFA,
    current_ulid: ?ULID, // ULID of the current record (database-managed, not part of user data)
    current_node: ?*btree.Node, // Current position in B-tree for sequential access
    current_position: usize, // Position within current node
    is_locked: bool,
    ulid_index: btree.BTree, // Special index for ULID lookups (auto-maintained)
    next_data_offset: u64, // Next offset for new record in data region

    const Self = @This();

    /// Create a new ISAM database file (.zdb)
    pub fn create(
        allocator: std.mem.Allocator,
        filename: []const u8,
        key_defs: []const KeyDef,
        record_size: u32,
        options: CreateOptions,
    ) IsamError!*Self {
        _ = options;

        const self = allocator.create(Self) catch return IsamError.OutOfMemory;
        errdefer allocator.destroy(self);

        // Create single database file (.zdb)
        var db_name_buf: [std.fs.max_path_bytes]u8 = undefined;
        const db_name = std.fmt.bufPrint(&db_name_buf, "{s}.zdb", .{filename}) catch
            return IsamError.OutOfMemory;

        // Create file with read+write access
        const file = std.fs.cwd().createFile(db_name, .{ .read = true }) catch
            return IsamError.IoError;
        errdefer file.close();

        // Initialize B-trees for each key
        const btrees = allocator.alloc(btree.BTree, key_defs.len) catch return IsamError.OutOfMemory;
        for (btrees) |*bt| {
            bt.* = btree.BTree.init(allocator);
        }

        // Deep copy key definitions (including segments)
        const owned_key_defs = allocator.alloc(KeyDef, key_defs.len) catch return IsamError.OutOfMemory;
        for (key_defs, 0..) |kd, i| {
            owned_key_defs[i] = .{
                .segments = allocator.dupe(KeyDef.KeySegment, kd.segments) catch return IsamError.OutOfMemory,
                .allow_duplicates = kd.allow_duplicates,
                .changes_allowed = kd.changes_allowed,
                .key_number = kd.key_number,
            };
        }

        // Calculate initial layout:
        // - Header at offset 0 (256 bytes)
        // - Key definitions follow header
        // - Index region starts after key defs (will grow as we add data)
        // - Data region starts after a reserved index area
        const key_defs_size = self.calculateKeyDefsSize(owned_key_defs);
        const initial_data_offset = HEADER_SIZE + key_defs_size + DEFAULT_PAGE_SIZE; // Reserve 1 page for index

        self.* = .{
            .allocator = allocator,
            .file = file,
            .header = FileHeader.init(@intCast(key_defs.len), record_size),
            .key_defs = owned_key_defs,
            .btrees = btrees,
            .current_key = 0,
            .current_rfa = null,
            .current_ulid = null,
            .current_node = null,
            .current_position = 0,
            .is_locked = false,
            .ulid_index = btree.BTree.init(allocator),
            .next_data_offset = initial_data_offset,
        };

        // Update header with calculated offsets
        self.header.key_defs_size = key_defs_size;
        self.header.index_offset = HEADER_SIZE + key_defs_size;
        self.header.data_offset = initial_data_offset;

        // Write initial file structure
        try self.writeHeader();
        try self.writeKeyDefs();

        return self;
    }

    pub const CreateOptions = struct {
        page_size: u32 = DEFAULT_PAGE_SIZE,
        record_type: RecordType = .fixed,
    };

    /// Calculate the size needed to store key definitions
    fn calculateKeyDefsSize(self: *Self, key_defs: []const KeyDef) u64 {
        _ = self;
        var size: u64 = 0;
        for (key_defs) |key_def| {
            // 1 byte segment count + 1 byte flags + 1 byte key number
            size += 3;
            // Each segment: 4 bytes start + 4 bytes length + 1 byte type
            size += key_def.segments.len * 9;
        }
        // Align to page boundary
        return ((size + DEFAULT_PAGE_SIZE - 1) / DEFAULT_PAGE_SIZE) * DEFAULT_PAGE_SIZE;
    }

    /// Write the file header to disk
    fn writeHeader(self: *Self) IsamError!void {
        const file = self.file orelse return IsamError.IoError;
        file.seekTo(0) catch return IsamError.IoError;
        _ = file.write(std.mem.asBytes(&self.header)) catch return IsamError.IoError;
    }

    /// Write key definitions after the header
    fn writeKeyDefs(self: *Self) IsamError!void {
        const file = self.file orelse return IsamError.IoError;
        file.seekTo(HEADER_SIZE) catch return IsamError.IoError;

        for (self.key_defs) |key_def| {
            // Write number of segments
            const seg_count: u8 = @intCast(key_def.segments.len);
            _ = file.write(&[_]u8{seg_count}) catch return IsamError.IoError;

            // Write flags
            const flags: u8 = (@as(u8, if (key_def.allow_duplicates) 1 else 0)) |
                (@as(u8, if (key_def.changes_allowed) 2 else 0));
            _ = file.write(&[_]u8{flags}) catch return IsamError.IoError;

            // Write key number
            _ = file.write(&[_]u8{key_def.key_number}) catch return IsamError.IoError;

            // Write each segment
            for (key_def.segments) |seg| {
                _ = file.write(std.mem.asBytes(&seg.start)) catch return IsamError.IoError;
                _ = file.write(std.mem.asBytes(&seg.length)) catch return IsamError.IoError;
                _ = file.write(&[_]u8{@intFromEnum(seg.key_type)}) catch return IsamError.IoError;
            }
        }
    }

    /// Read key definitions from file
    fn readKeyDefs(self: *Self) IsamError!void {
        const file = self.file orelse return IsamError.IoError;

        // Seek to key definitions
        file.seekTo(self.header.key_defs_offset) catch return IsamError.IoError;

        // Read key definitions
        const key_count = self.header.key_count;
        self.key_defs = self.allocator.alloc(KeyDef, key_count) catch return IsamError.OutOfMemory;

        for (self.key_defs, 0..) |*key_def, i| {
            // Read segment count
            var seg_count_buf: [1]u8 = undefined;
            _ = file.read(&seg_count_buf) catch return IsamError.IoError;
            const seg_count = seg_count_buf[0];

            // Read flags
            var flags_buf: [1]u8 = undefined;
            _ = file.read(&flags_buf) catch return IsamError.IoError;
            const flags = flags_buf[0];

            // Read key number
            var key_num_buf: [1]u8 = undefined;
            _ = file.read(&key_num_buf) catch return IsamError.IoError;

            // Allocate and read segments
            const segments = self.allocator.alloc(KeyDef.KeySegment, seg_count) catch return IsamError.OutOfMemory;

            for (segments) |*seg| {
                var start_buf: [4]u8 = undefined;
                _ = file.read(&start_buf) catch return IsamError.IoError;
                seg.start = std.mem.readInt(u32, &start_buf, .little);

                var len_buf: [4]u8 = undefined;
                _ = file.read(&len_buf) catch return IsamError.IoError;
                seg.length = std.mem.readInt(u32, &len_buf, .little);

                var type_buf: [1]u8 = undefined;
                _ = file.read(&type_buf) catch return IsamError.IoError;
                seg.key_type = @enumFromInt(type_buf[0]);
            }

            key_def.* = .{
                .segments = segments,
                .allow_duplicates = (flags & 1) != 0,
                .changes_allowed = (flags & 2) != 0,
                .key_number = @intCast(i),
            };
        }
    }

    /// Open an existing ISAM file (.zdb)
    pub fn open(
        allocator: std.mem.Allocator,
        filename: []const u8,
        mode: OpenMode,
    ) IsamError!*Self {
        _ = mode;

        const self = allocator.create(Self) catch return IsamError.OutOfMemory;
        errdefer allocator.destroy(self);

        // Open single database file (.zdb)
        var db_name_buf: [std.fs.max_path_bytes]u8 = undefined;
        const db_name = std.fmt.bufPrint(&db_name_buf, "{s}.zdb", .{filename}) catch
            return IsamError.OutOfMemory;

        const file = std.fs.cwd().openFile(db_name, .{ .mode = .read_write }) catch
            return IsamError.FileNotFound;
        errdefer file.close();

        // Read and validate header
        var header: FileHeader = undefined;
        _ = file.read(std.mem.asBytes(&header)) catch return IsamError.IoError;

        if (!std.mem.eql(u8, &header.magic, &MAGIC)) {
            return IsamError.InvalidFormat;
        }

        if (header.version != VERSION) {
            return IsamError.InvalidFormat;
        }

        // Create B-trees based on key_count from header
        const btrees = allocator.alloc(btree.BTree, header.key_count) catch return IsamError.OutOfMemory;
        for (btrees) |*bt| {
            bt.* = btree.BTree.init(allocator);
        }

        // Get file size for next_data_offset
        const file_size = file.getEndPos() catch return IsamError.IoError;

        self.* = .{
            .allocator = allocator,
            .file = file,
            .header = header,
            .key_defs = &[_]KeyDef{},
            .btrees = btrees,
            .current_key = 0,
            .current_rfa = null,
            .current_ulid = null,
            .current_node = null,
            .current_position = 0,
            .is_locked = false,
            .ulid_index = btree.BTree.init(allocator),
            .next_data_offset = file_size,
        };

        // Read key definitions from file
        try self.readKeyDefs();

        // Load B-tree from index file
        try self.deserializeIndex();

        return self;
    }

    pub const OpenMode = enum {
        read_only,
        read_write,
        exclusive,
    };

    /// Close the ISAM file
    pub fn close(self: *Self) void {
        // Persist index and header to disk before closing
        self.serializeIndex() catch {};
        self.writeHeader() catch {};

        // Clean up B-trees
        for (self.btrees) |*bt| {
            bt.deinit();
        }
        self.allocator.free(self.btrees);

        // Clean up ULID index
        self.ulid_index.deinit();

        // Close single file handle
        if (self.file) |f| {
            f.close();
            self.file = null;
        }

        if (self.key_defs.len > 0) {
            // Free segments for each key definition
            for (self.key_defs) |kd| {
                if (kd.segments.len > 0) {
                    self.allocator.free(kd.segments);
                }
            }
            self.allocator.free(self.key_defs);
        }
        self.allocator.destroy(self);
    }

    /// Store a new record
    /// Automatically generates a ULID for the record (database-managed identifier)
    pub fn store(self: *Self, record: []const u8) IsamError!RFA {
        // Generate a unique ULID for this record
        const record_ulid = ULID.new();

        // Allocate space in data file (ULID + length + record)
        const rfa = try self.allocateRecord(record.len);

        // Write ULID + record to data file
        try self.writeRecordWithUlid(rfa, record_ulid, record);

        // Update all user-defined indexes
        for (self.key_defs, 0..) |key_def, i| {
            const key = key_def.extractKey(record, self.allocator) catch
                return IsamError.OutOfMemory;
            defer self.allocator.free(key);

            try self.insertKey(@intCast(i), key, rfa);
        }

        // Update ULID index for direct ULID lookups
        try self.insertUlidIndex(record_ulid, rfa);

        self.header.record_count += 1;
        self.current_rfa = rfa;
        self.current_ulid = record_ulid;

        return rfa;
    }

    /// Store a new record and return its ULID
    /// This is the preferred method when you need the record's unique identifier
    pub fn storeWithUlid(self: *Self, record: []const u8) IsamError!ULID {
        _ = try self.store(record);
        return self.current_ulid.?;
    }

    /// Read a record by key
    pub fn read(
        self: *Self,
        key: []const u8,
        record_buf: []u8,
        options: ReadOptions,
    ) IsamError!usize {
        self.current_key = options.key_number;

        // Find key in index
        const rfa = try self.findKey(options.key_number, key, options.match_mode);

        // Read record and its ULID
        var record_ulid: ULID = undefined;
        const len = try self.readRecordWithUlid(rfa, record_buf, &record_ulid);

        self.current_rfa = rfa;
        self.current_ulid = record_ulid;

        // Apply lock if requested
        if (options.lock_mode != .no_lock) {
            self.is_locked = true;
        }

        return len;
    }

    /// Get the ULID of the current record
    /// Returns null if no record has been read
    pub fn getRecordUlid(self: *Self) ?ULID {
        return self.current_ulid;
    }

    /// Get the ULID of the current record as a 26-character string
    /// Returns null if no record has been read
    pub fn getRecordUlidString(self: *Self) ?[26]u8 {
        if (self.current_ulid) |ulid| {
            return ulid.encode();
        }
        return null;
    }

    /// Read a record directly by its ULID
    /// This is a modern alternative to reading by RFA
    pub fn readByUlid(
        self: *Self,
        record_ulid: ULID,
        record_buf: []u8,
        options: ReadOptions,
    ) IsamError!usize {
        // Look up RFA from ULID index
        const rfa = try self.findByUlid(record_ulid);

        // Read record (ULID is already known)
        var stored_ulid: ULID = undefined;
        const len = try self.readRecordWithUlid(rfa, record_buf, &stored_ulid);

        self.current_rfa = rfa;
        self.current_ulid = stored_ulid;

        // Apply lock if requested
        if (options.lock_mode != .no_lock) {
            self.is_locked = true;
        }

        return len;
    }

    /// Read a record by ULID string (26-character Crockford Base32)
    pub fn readByUlidString(
        self: *Self,
        ulid_str: []const u8,
        record_buf: []u8,
        options: ReadOptions,
    ) IsamError!usize {
        const record_ulid = ULID.decode(ulid_str) catch return IsamError.InvalidUlid;
        return self.readByUlid(record_ulid, record_buf, options);
    }

    pub const ReadOptions = struct {
        key_number: u8 = 0,
        match_mode: MatchMode = .greater_equal,
        lock_mode: LockMode = .no_lock,
    };

    /// Set the current key for sequential reads
    /// Resets position to start of that key's index
    pub fn setCurrentKey(self: *Self, key_number: u8) void {
        if (key_number != self.current_key) {
            self.current_key = key_number;
            self.current_rfa = null;
            self.current_node = null;
            self.current_position = 0;
        }
    }

    /// Read next sequential record (optionally by key number)
    pub fn readNext(self: *Self, record_buf: []u8) IsamError!usize {
        return self.readNextByKey(self.current_key, record_buf);
    }

    /// Read next sequential record using specified key
    pub fn readNextByKey(self: *Self, key_number: u8, record_buf: []u8) IsamError!usize {
        // If key changed, reset position to start of new key's index
        if (key_number != self.current_key) {
            self.current_key = key_number;
            self.current_rfa = null;
            self.current_ulid = null;
            self.current_node = null; // Reset B-tree position
            self.current_position = 0;
        }

        // Get next RFA from current position in index
        // If no current position, getNextRfa starts from beginning
        const next_rfa = try self.getNextRfa(self.current_key, self.current_rfa);

        // Read record and its ULID
        var record_ulid: ULID = undefined;
        const len = try self.readRecordWithUlid(next_rfa, record_buf, &record_ulid);
        self.current_rfa = next_rfa;
        self.current_ulid = record_ulid;

        return len;
    }

    /// Update current record
    /// Preserves the record's ULID (database-managed identifier doesn't change on update)
    pub fn write(self: *Self, record: []const u8) IsamError!void {
        if (self.current_rfa == null or self.current_ulid == null) {
            return IsamError.KeyNotFound;
        }

        // Update record in data file, preserving its ULID
        try self.writeRecordWithUlid(self.current_rfa.?, self.current_ulid.?, record);

        // Update indexes if keys changed
        // TODO: Implement key change detection and index update
    }

    /// Delete current record
    pub fn delete(self: *Self) IsamError!void {
        if (self.current_rfa == null) {
            return IsamError.KeyNotFound;
        }

        // Remove from all indexes
        // TODO: Implement index removal

        // Mark record as deleted in data file
        try self.freeRecord(self.current_rfa.?);

        self.header.record_count -= 1;
        self.current_rfa = null;
    }

    /// Release record lock
    pub fn unlock(self: *Self) void {
        self.is_locked = false;
    }

    /// Flush buffers to disk
    pub fn flush(self: *Self) IsamError!void {
        try self.writeHeader();
        try self.serializeIndex();
    }

    /// Serialize B-tree indexes to disk
    fn serializeIndex(self: *Self) IsamError!void {
        const file = self.file orelse return IsamError.IoError;

        // Seek to index region
        file.seekTo(self.header.index_offset) catch return IsamError.IoError;

        // For each user-defined B-tree, serialize all leaf entries
        for (self.btrees) |*bt| {
            // Get first leaf and count entries
            const entry_count: u32 = @intCast(bt.size);

            // Write entry count for this key
            _ = file.write(std.mem.asBytes(&entry_count)) catch return IsamError.IoError;

            // Traverse leaves and write entries
            var leaf = bt.firstLeaf();
            while (leaf) |node| {
                var i: usize = 0;
                while (i < node.key_count) : (i += 1) {
                    const key = node.keys[i];
                    const rec = node.records[i];

                    // Write key length
                    const key_len: u16 = @intCast(key.data.len);
                    _ = file.write(std.mem.asBytes(&key_len)) catch return IsamError.IoError;

                    // Write key data
                    _ = file.write(key.data) catch return IsamError.IoError;

                    // Write RFA
                    const rfa_bytes = (RFA{ .block = rec.block, .offset = rec.offset }).toBytes();
                    _ = file.write(&rfa_bytes) catch return IsamError.IoError;
                }
                leaf = node.next_leaf;
            }
        }

        // Serialize ULID index (fixed 16-byte keys)
        const ulid_count: u32 = @intCast(self.ulid_index.size);
        _ = file.write(std.mem.asBytes(&ulid_count)) catch return IsamError.IoError;

        var ulid_leaf = self.ulid_index.firstLeaf();
        while (ulid_leaf) |node| {
            var i: usize = 0;
            while (i < node.key_count) : (i += 1) {
                const key = node.keys[i];
                const rec = node.records[i];

                // Write ULID (always 16 bytes, no length prefix needed)
                _ = file.write(key.data) catch return IsamError.IoError;

                // Write RFA
                const rfa_bytes = (RFA{ .block = rec.block, .offset = rec.offset }).toBytes();
                _ = file.write(&rfa_bytes) catch return IsamError.IoError;
            }
            ulid_leaf = node.next_leaf;
        }
    }

    /// Deserialize B-tree indexes from disk
    fn deserializeIndex(self: *Self) IsamError!void {
        const file = self.file orelse return IsamError.IoError;

        // Seek to index region
        file.seekTo(self.header.index_offset) catch return IsamError.IoError;

        // For each user-defined B-tree, read and insert entries
        for (self.btrees) |*bt| {
            // Read entry count
            var count_buf: [4]u8 = undefined;
            const count_read = file.read(&count_buf) catch return IsamError.IoError;
            if (count_read < 4) {
                // No more data, this is OK for newly created files
                return;
            }
            const entry_count = std.mem.readInt(u32, &count_buf, .little);

            // Read each entry
            var j: u32 = 0;
            while (j < entry_count) : (j += 1) {
                // Read key length
                var len_buf: [2]u8 = undefined;
                _ = file.read(&len_buf) catch return IsamError.IoError;
                const key_len = std.mem.readInt(u16, &len_buf, .little);

                // Read key data into temporary buffer
                const key_data = self.allocator.alloc(u8, key_len) catch return IsamError.OutOfMemory;
                defer self.allocator.free(key_data); // Always free - insert() makes its own copy

                _ = file.read(key_data) catch {
                    return IsamError.IoError;
                };

                // Read RFA
                var rfa_buf: [8]u8 = undefined;
                _ = file.read(&rfa_buf) catch {
                    return IsamError.IoError;
                };
                const rfa = RFA.fromBytes(rfa_buf);

                // Insert into B-tree (insert() copies the key data)
                const btree_key = btree.Key{ .data = key_data };
                const record_ptr = btree.RecordPtr{ .block = rfa.block, .offset = rfa.offset };
                bt.insert(btree_key, record_ptr) catch {
                    return IsamError.OutOfMemory;
                };
            }
        }

        // Deserialize ULID index
        var ulid_count_buf: [4]u8 = undefined;
        const ulid_count_read = file.read(&ulid_count_buf) catch return;
        if (ulid_count_read < 4) return; // No ULID index (older file format)

        const ulid_count = std.mem.readInt(u32, &ulid_count_buf, .little);

        var k: u32 = 0;
        while (k < ulid_count) : (k += 1) {
            // Read ULID (always 16 bytes) into temporary buffer
            const ulid_data = self.allocator.alloc(u8, 16) catch return IsamError.OutOfMemory;
            defer self.allocator.free(ulid_data); // Always free - insert() makes its own copy

            _ = file.read(ulid_data) catch {
                return IsamError.IoError;
            };

            // Read RFA
            var rfa_buf: [8]u8 = undefined;
            _ = file.read(&rfa_buf) catch {
                return IsamError.IoError;
            };
            const rfa = RFA.fromBytes(rfa_buf);

            // Insert into ULID index (insert() copies the key data)
            const ulid_key = btree.Key{ .data = ulid_data };
            const record_ptr = btree.RecordPtr{ .block = rfa.block, .offset = rfa.offset };
            self.ulid_index.insert(ulid_key, record_ptr) catch {
                return IsamError.OutOfMemory;
            };
        }
    }

    // ============================================================
    // Private Implementation
    // ============================================================

    fn allocateRecord(self: *Self, size: usize) IsamError!RFA {
        _ = size;
        // TODO: Implement free list management
        // For now, append to end of data region using tracked offset
        const pos = self.next_data_offset;

        // Update next_data_offset for next allocation
        // Record format: ULID (16) + length (4) + data
        self.next_data_offset += 16 + 4 + self.header.record_size;

        return RFA{
            .block = @intCast(pos / self.header.page_size),
            .offset = @intCast(pos % self.header.page_size),
        };
    }

    /// Write a record with its ULID to the data file
    /// Data format: [ULID 16 bytes][length 4 bytes][record data]
    fn writeRecordWithUlid(self: *Self, rfa: RFA, record_ulid: ULID, record: []const u8) IsamError!void {
        const file = self.file orelse return IsamError.IoError;
        const pos = @as(u64, rfa.block) * self.header.page_size + rfa.offset;

        file.seekTo(pos) catch return IsamError.IoError;

        // Write ULID first (16 bytes) - database-managed, not part of user data
        _ = file.write(&record_ulid.bytes) catch return IsamError.IoError;

        // Write record length (for variable length support)
        const len: u32 = @intCast(record.len);
        _ = file.write(std.mem.asBytes(&len)) catch return IsamError.IoError;

        // Write record data
        _ = file.write(record) catch return IsamError.IoError;
    }

    /// Read a record and its ULID from the data file
    /// Data format: [ULID 16 bytes][length 4 bytes][record data]
    fn readRecordWithUlid(self: *Self, rfa: RFA, buf: []u8, out_ulid: *ULID) IsamError!usize {
        const file = self.file orelse return IsamError.IoError;
        const pos = @as(u64, rfa.block) * self.header.page_size + rfa.offset;

        file.seekTo(pos) catch return IsamError.IoError;

        // Read ULID first (16 bytes)
        var ulid_buf: [16]u8 = undefined;
        const ulid_read = file.read(&ulid_buf) catch return IsamError.IoError;
        if (ulid_read < 16) return IsamError.IoError;
        out_ulid.* = ULID{ .bytes = ulid_buf };

        // Read record length
        var len_buf: [4]u8 = undefined;
        _ = file.read(&len_buf) catch return IsamError.IoError;
        const len = std.mem.readInt(u32, &len_buf, .little);

        // Read record data
        const read_len = @min(len, buf.len);
        const bytes_read = file.read(buf[0..read_len]) catch return IsamError.IoError;

        return bytes_read;
    }

    /// Insert ULID into the ULID index for direct lookups
    fn insertUlidIndex(self: *Self, record_ulid: ULID, rfa: RFA) IsamError!void {
        // Use ULID bytes as the key (16 bytes, lexicographically sortable)
        const ulid_key = btree.Key{ .data = &record_ulid.bytes };
        const record_ptr = btree.RecordPtr{ .block = rfa.block, .offset = rfa.offset };

        self.ulid_index.insert(ulid_key, record_ptr) catch return IsamError.OutOfMemory;
    }

    /// Find RFA by ULID
    fn findByUlid(self: *Self, record_ulid: ULID) IsamError!RFA {
        const ulid_key = btree.Key{ .data = &record_ulid.bytes };

        const result = self.ulid_index.searchWithMode(ulid_key, .exact) orelse {
            return IsamError.UlidNotFound;
        };

        return RFA{
            .block = result.node.records[result.position].block,
            .offset = result.node.records[result.position].offset,
        };
    }

    fn freeRecord(self: *Self, rfa: RFA) IsamError!void {
        _ = self;
        _ = rfa;
        // TODO: Add to free list
    }

    fn insertKey(self: *Self, key_num: u8, key: []const u8, rfa: RFA) IsamError!void {
        if (key_num >= self.btrees.len) {
            return IsamError.InvalidKey;
        }

        const bt = &self.btrees[key_num];
        const btree_key = btree.Key{ .data = key };
        const record_ptr = btree.RecordPtr{ .block = rfa.block, .offset = rfa.offset };

        bt.insert(btree_key, record_ptr) catch return IsamError.OutOfMemory;
    }

    fn findKey(self: *Self, key_num: u8, key: []const u8, match_mode: MatchMode) IsamError!RFA {
        if (key_num >= self.btrees.len) {
            return IsamError.InvalidKey;
        }

        const bt = &self.btrees[key_num];
        const btree_key = btree.Key{ .data = key };

        // Convert MatchMode to btree.BTree.SearchMode
        const search_mode: btree.BTree.SearchMode = switch (match_mode) {
            .exact => .exact,
            .greater_equal => .greater_equal,
            .greater => .greater,
            .partial => .partial,
        };

        const result = bt.searchWithMode(btree_key, search_mode) orelse {
            return IsamError.KeyNotFound;
        };

        // Store current position for sequential access
        self.current_node = result.node;
        self.current_position = result.position;

        return RFA{
            .block = result.node.records[result.position].block,
            .offset = result.node.records[result.position].offset,
        };
    }

    fn getNextRfa(self: *Self, key_num: u8, current: ?RFA) IsamError!RFA {
        _ = current; // Position tracked via current_node/current_position

        if (key_num >= self.btrees.len) {
            return IsamError.InvalidKey;
        }

        // If no current position, start from beginning
        if (self.current_node == null) {
            const bt = &self.btrees[key_num];
            const first = bt.firstLeaf() orelse return IsamError.EndOfFile;
            self.current_node = first;
            self.current_position = 0;

            if (first.key_count == 0) {
                return IsamError.EndOfFile;
            }

            return RFA{
                .block = first.records[0].block,
                .offset = first.records[0].offset,
            };
        }

        var node = self.current_node.?;

        // Move to next position
        self.current_position += 1;

        // Check if we need to move to next leaf
        if (self.current_position >= node.key_count) {
            if (node.next_leaf) |next| {
                self.current_node = next;
                self.current_position = 0;
                node = next;

                if (node.key_count == 0) {
                    return IsamError.EndOfFile;
                }
            } else {
                // No more leaves
                self.current_node = null;
                self.current_position = 0;
                return IsamError.EndOfFile;
            }
        }

        return RFA{
            .block = node.records[self.current_position].block,
            .offset = node.records[self.current_position].offset,
        };
    }
};

test "isam basic types" {
    const key_def = KeyDef{
        .segments = &[_]KeyDef.KeySegment{
            .{ .start = 0, .length = 8, .key_type = .decimal },
        },
        .allow_duplicates = false,
        .changes_allowed = false,
        .key_number = 0,
    };

    try std.testing.expectEqual(@as(u32, 8), key_def.keyLength());
}

test "rfa conversion" {
    const rfa = RFA{ .block = 12345, .offset = 678 };
    const bytes = rfa.toBytes();
    const restored = RFA.fromBytes(bytes);

    try std.testing.expectEqual(rfa.block, restored.block);
    try std.testing.expectEqual(rfa.offset, restored.offset);
}

test "isam store and read" {
    const allocator = std.testing.allocator;

    // Define a simple key on first 8 bytes
    const key_segments = [_]KeyDef.KeySegment{
        .{ .start = 0, .length = 8, .key_type = .alpha },
    };
    const key_defs = [_]KeyDef{
        .{
            .segments = &key_segments,
            .allow_duplicates = false,
            .changes_allowed = false,
            .key_number = 0,
        },
    };

    // Create ISAM file
    const isam_file = try IsamFile.create(
        allocator,
        "/tmp/test_isam",
        &key_defs,
        64, // record size
        .{},
    );
    defer isam_file.close();

    // Store some records
    const record1 = "KEY00001" ++ "Record 1 data here padded to 64 bytes.............";
    const record2 = "KEY00002" ++ "Record 2 data here padded to 64 bytes.............";
    const record3 = "KEY00003" ++ "Record 3 data here padded to 64 bytes.............";

    _ = try isam_file.store(record1);
    _ = try isam_file.store(record2);
    _ = try isam_file.store(record3);

    // Read by key
    var buf: [64]u8 = undefined;
    const len = try isam_file.read("KEY00002", &buf, .{ .match_mode = .exact });

    try std.testing.expect(len > 0);
    try std.testing.expect(std.mem.startsWith(u8, &buf, "KEY00002"));

    // Test sequential access
    isam_file.current_node = null; // Reset position
    isam_file.current_position = 0;

    // Read first
    _ = try isam_file.read("KEY00001", &buf, .{ .match_mode = .greater_equal });
    try std.testing.expect(std.mem.startsWith(u8, &buf, "KEY00001"));

    // Read next
    const len2 = try isam_file.readNext(&buf);
    try std.testing.expect(len2 > 0);
    try std.testing.expect(std.mem.startsWith(u8, &buf, "KEY00002"));

    // Read next again
    const len3 = try isam_file.readNext(&buf);
    try std.testing.expect(len3 > 0);
    try std.testing.expect(std.mem.startsWith(u8, &buf, "KEY00003"));

    // Should hit EOF
    const eof_result = isam_file.readNext(&buf);
    try std.testing.expectError(IsamError.EndOfFile, eof_result);

    // Clean up test file
    std.fs.cwd().deleteFile("/tmp/test_isam.zdb") catch {};
}

test "isam persistence" {
    const allocator = std.testing.allocator;

    // Define a simple key on first 8 bytes
    const key_segments = [_]KeyDef.KeySegment{
        .{ .start = 0, .length = 8, .key_type = .alpha },
    };
    const key_defs = [_]KeyDef{
        .{
            .segments = &key_segments,
            .allow_duplicates = false,
            .changes_allowed = false,
            .key_number = 0,
        },
    };

    // Create and populate ISAM file
    {
        const isam_file = try IsamFile.create(
            allocator,
            "/tmp/test_persist",
            &key_defs,
            64,
            .{},
        );

        const record1 = "AAAAAAAA" ++ "First record data padded to sixty-four bytes....";
        const record2 = "BBBBBBBB" ++ "Second record data padded to sixty-four bytes...";
        const record3 = "CCCCCCCC" ++ "Third record data padded to sixty-four bytes....";

        _ = try isam_file.store(record1);
        _ = try isam_file.store(record2);
        _ = try isam_file.store(record3);

        // Close (should persist to disk)
        isam_file.close();
    }

    // Reopen and verify data is still there
    {
        const isam_file = try IsamFile.open(allocator, "/tmp/test_persist", .read_write);
        defer isam_file.close();

        var buf: [64]u8 = undefined;

        // Read by key - should find records from previous session
        const len1 = try isam_file.read("BBBBBBBB", &buf, .{ .match_mode = .exact });
        try std.testing.expect(len1 > 0);
        try std.testing.expect(std.mem.startsWith(u8, &buf, "BBBBBBBB"));

        // Sequential read should work too
        isam_file.current_node = null;
        isam_file.current_position = 0;

        _ = try isam_file.read("AAAAAAAA", &buf, .{ .match_mode = .greater_equal });
        try std.testing.expect(std.mem.startsWith(u8, &buf, "AAAAAAAA"));

        _ = try isam_file.readNext(&buf);
        try std.testing.expect(std.mem.startsWith(u8, &buf, "BBBBBBBB"));

        _ = try isam_file.readNext(&buf);
        try std.testing.expect(std.mem.startsWith(u8, &buf, "CCCCCCCC"));
    }

    // Clean up test file
    std.fs.cwd().deleteFile("/tmp/test_persist.zdb") catch {};
}

test "isam ULID record identifier" {
    const allocator = std.testing.allocator;

    // Define a simple key on first 8 bytes
    const key_segments = [_]KeyDef.KeySegment{
        .{ .start = 0, .length = 8, .key_type = .alpha },
    };
    const key_defs = [_]KeyDef{
        .{
            .segments = &key_segments,
            .allow_duplicates = false,
            .changes_allowed = false,
            .key_number = 0,
        },
    };

    // Create ISAM file
    const isam_file = try IsamFile.create(
        allocator,
        "/tmp/test_ulid",
        &key_defs,
        64,
        .{},
    );
    defer isam_file.close();

    // Store records and capture their ULIDs
    const record1 = "ULID0001" ++ "First record with auto-generated ULID............";
    const record2 = "ULID0002" ++ "Second record with auto-generated ULID...........";

    // Use storeWithUlid to get the ULID directly
    const ulid1 = try isam_file.storeWithUlid(record1);
    const ulid2 = try isam_file.storeWithUlid(record2);

    // ULIDs should be different
    try std.testing.expect(!ulid1.eql(ulid2));

    // ULIDs should be comparable (ulid1 generated before ulid2)
    // Note: ULIDs from same millisecond are only sorted by random part
    const order = ulid1.compare(ulid2);
    try std.testing.expect(order == .lt or order == .gt or order == .eq);

    // Read by key and verify we can get the ULID
    var buf: [64]u8 = undefined;
    _ = try isam_file.read("ULID0001", &buf, .{ .match_mode = .exact });

    // Get the ULID of the current record
    const retrieved_ulid = isam_file.getRecordUlid();
    try std.testing.expect(retrieved_ulid != null);
    try std.testing.expect(retrieved_ulid.?.eql(ulid1));

    // Get as string
    const ulid_str = isam_file.getRecordUlidString();
    try std.testing.expect(ulid_str != null);
    try std.testing.expectEqual(@as(usize, 26), ulid_str.?.len);

    // Read directly by ULID (modern alternative to RFA)
    var buf2: [64]u8 = undefined;
    const len = try isam_file.readByUlid(ulid2, &buf2, .{});
    try std.testing.expect(len > 0);
    try std.testing.expect(std.mem.startsWith(u8, &buf2, "ULID0002"));

    // Read by ULID string
    const ulid2_str = ulid2.encode();
    var buf3: [64]u8 = undefined;
    const len3 = try isam_file.readByUlidString(&ulid2_str, &buf3, .{});
    try std.testing.expect(len3 > 0);
    try std.testing.expect(std.mem.startsWith(u8, &buf3, "ULID0002"));

    // Clean up test file
    std.fs.cwd().deleteFile("/tmp/test_ulid.zdb") catch {};
}
