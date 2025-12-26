//! Ziggy ISAM - Indexed Sequential Access Method
//!
//! A Zig implementation of ISAM file storage, compatible with
//! Synergy DBL ISAM semantics.

const std = @import("std");
const btree = @import("btree.zig");

/// ISAM file magic number
pub const MAGIC_INDEX = [8]u8{ 'Z', 'I', 'G', 'G', 'Y', 'I', 'D', 'X' };
pub const MAGIC_DATA = [8]u8{ 'Z', 'I', 'G', 'G', 'Y', 'D', 'A', 'T' };

/// Current file format version
pub const VERSION: u32 = 1;

/// Default block size
pub const DEFAULT_BLOCK_SIZE: u32 = 4096;

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

/// ISAM file header (stored at start of .ism file)
pub const IndexHeader = struct {
    magic: [8]u8,
    version: u32,
    block_size: u32,
    key_count: u16,
    root_block: u64,
    record_count: u64,
    flags: u32,
    // Key definitions follow
};

/// ISAM data file header (stored at start of .is1 file)
pub const DataHeader = struct {
    magic: [8]u8,
    version: u32,
    record_type: RecordType,
    record_size: u32, // For fixed-length
    free_list_head: u64,
    record_count: u64,
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
    index_file: ?std.fs.File,
    data_file: ?std.fs.File,
    index_header: IndexHeader,
    data_header: DataHeader,
    key_defs: []KeyDef,
    btrees: []btree.BTree, // One B-tree per key definition
    current_key: u8,
    current_rfa: ?RFA,
    current_node: ?*btree.Node, // Current position in B-tree for sequential access
    current_position: usize, // Position within current node
    is_locked: bool,
    buffer_pool: BufferPool,

    const Self = @This();

    /// Create a new ISAM file
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

        // Create index filename (.ism)
        var index_name_buf: [std.fs.max_path_bytes]u8 = undefined;
        const index_name = std.fmt.bufPrint(&index_name_buf, "{s}.ism", .{filename}) catch
            return IsamError.OutOfMemory;

        // Create data filename (.is1)
        var data_name_buf: [std.fs.max_path_bytes]u8 = undefined;
        const data_name = std.fmt.bufPrint(&data_name_buf, "{s}.is1", .{filename}) catch
            return IsamError.OutOfMemory;

        // Create files with read+write access
        const index_file = std.fs.cwd().createFile(index_name, .{ .read = true }) catch
            return IsamError.IoError;
        errdefer index_file.close();

        const data_file = std.fs.cwd().createFile(data_name, .{ .read = true }) catch
            return IsamError.IoError;
        errdefer data_file.close();

        // Initialize headers
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

        self.* = .{
            .allocator = allocator,
            .index_file = index_file,
            .data_file = data_file,
            .index_header = .{
                .magic = MAGIC_INDEX,
                .version = VERSION,
                .block_size = DEFAULT_BLOCK_SIZE,
                .key_count = @intCast(key_defs.len),
                .root_block = 0,
                .record_count = 0,
                .flags = 0,
            },
            .data_header = .{
                .magic = MAGIC_DATA,
                .version = VERSION,
                .record_type = .fixed,
                .record_size = record_size,
                .free_list_head = 0,
                .record_count = 0,
            },
            .key_defs = owned_key_defs,
            .btrees = btrees,
            .current_key = 0,
            .current_rfa = null,
            .current_node = null,
            .current_position = 0,
            .is_locked = false,
            .buffer_pool = BufferPool.init(allocator),
        };

        // Write headers
        try self.writeHeaders();

        return self;
    }

    pub const CreateOptions = struct {
        block_size: u32 = DEFAULT_BLOCK_SIZE,
        record_type: RecordType = .fixed,
    };

    /// Open an existing ISAM file
    pub fn open(
        allocator: std.mem.Allocator,
        filename: []const u8,
        mode: OpenMode,
    ) IsamError!*Self {
        _ = mode;

        const self = allocator.create(Self) catch return IsamError.OutOfMemory;
        errdefer allocator.destroy(self);

        // Open index file
        var index_name_buf: [std.fs.max_path_bytes]u8 = undefined;
        const index_name = std.fmt.bufPrint(&index_name_buf, "{s}.ism", .{filename}) catch
            return IsamError.OutOfMemory;

        var data_name_buf: [std.fs.max_path_bytes]u8 = undefined;
        const data_name = std.fmt.bufPrint(&data_name_buf, "{s}.is1", .{filename}) catch
            return IsamError.OutOfMemory;

        const index_file = std.fs.cwd().openFile(index_name, .{ .mode = .read_write }) catch
            return IsamError.FileNotFound;
        errdefer index_file.close();

        const data_file = std.fs.cwd().openFile(data_name, .{ .mode = .read_write }) catch
            return IsamError.FileNotFound;
        errdefer data_file.close();

        // Read and validate headers
        var index_header: IndexHeader = undefined;
        _ = index_file.read(std.mem.asBytes(&index_header)) catch return IsamError.IoError;

        if (!std.mem.eql(u8, &index_header.magic, &MAGIC_INDEX)) {
            return IsamError.InvalidFormat;
        }

        var data_header: DataHeader = undefined;
        _ = data_file.read(std.mem.asBytes(&data_header)) catch return IsamError.IoError;

        if (!std.mem.eql(u8, &data_header.magic, &MAGIC_DATA)) {
            return IsamError.InvalidFormat;
        }

        // Create B-trees based on key_count from header
        const btrees = allocator.alloc(btree.BTree, index_header.key_count) catch return IsamError.OutOfMemory;
        for (btrees) |*bt| {
            bt.* = btree.BTree.init(allocator);
        }

        self.* = .{
            .allocator = allocator,
            .index_file = index_file,
            .data_file = data_file,
            .index_header = index_header,
            .data_header = data_header,
            .key_defs = &[_]KeyDef{},
            .btrees = btrees,
            .current_key = 0,
            .current_rfa = null,
            .current_node = null,
            .current_position = 0,
            .is_locked = false,
            .buffer_pool = BufferPool.init(allocator),
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
        // Persist index to disk before closing
        self.serializeIndex() catch {};
        self.writeHeaders() catch {};

        self.buffer_pool.flush() catch {};
        self.buffer_pool.deinit();

        // Clean up B-trees
        for (self.btrees) |*bt| {
            bt.deinit();
        }
        self.allocator.free(self.btrees);

        if (self.index_file) |*f| {
            f.close();
            self.index_file = null;
        }
        if (self.data_file) |*f| {
            f.close();
            self.data_file = null;
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
    pub fn store(self: *Self, record: []const u8) IsamError!RFA {
        // Allocate space in data file
        const rfa = try self.allocateRecord(record.len);

        // Write record
        try self.writeRecord(rfa, record);

        // Update all indexes
        for (self.key_defs, 0..) |key_def, i| {
            const key = key_def.extractKey(record, self.allocator) catch
                return IsamError.OutOfMemory;
            defer self.allocator.free(key);

            try self.insertKey(@intCast(i), key, rfa);
        }

        self.data_header.record_count += 1;
        self.current_rfa = rfa;

        return rfa;
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

        // Read record
        const len = try self.readRecord(rfa, record_buf);

        self.current_rfa = rfa;

        // Apply lock if requested
        if (options.lock_mode != .no_lock) {
            self.is_locked = true;
        }

        return len;
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
            self.current_node = null; // Reset B-tree position
            self.current_position = 0;
        }

        // Get next RFA from current position in index
        // If no current position, getNextRfa starts from beginning
        const next_rfa = try self.getNextRfa(self.current_key, self.current_rfa);

        const len = try self.readRecord(next_rfa, record_buf);
        self.current_rfa = next_rfa;

        return len;
    }

    /// Update current record
    pub fn write(self: *Self, record: []const u8) IsamError!void {
        if (self.current_rfa == null) {
            return IsamError.KeyNotFound;
        }

        // Update record in data file
        try self.writeRecord(self.current_rfa.?, record);

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

        self.data_header.record_count -= 1;
        self.current_rfa = null;
    }

    /// Release record lock
    pub fn unlock(self: *Self) void {
        self.is_locked = false;
    }

    /// Flush buffers to disk
    pub fn flush(self: *Self) IsamError!void {
        try self.buffer_pool.flush();
        try self.writeHeaders();
        try self.serializeIndex();
    }

    /// Serialize B-tree indexes to disk
    fn serializeIndex(self: *Self) IsamError!void {
        const index_file = self.index_file orelse return IsamError.IoError;

        // Seek past header (reserve space for header + key defs)
        const header_size: u64 = DEFAULT_BLOCK_SIZE;
        index_file.seekTo(header_size) catch return IsamError.IoError;

        // For each B-tree, serialize all leaf entries
        for (self.btrees, 0..) |*bt, key_idx| {
            // Get first leaf and count entries
            var entry_count: u32 = @intCast(bt.size);

            // Write entry count for this key
            _ = index_file.write(std.mem.asBytes(&entry_count)) catch return IsamError.IoError;

            // Traverse leaves and write entries
            var leaf = bt.firstLeaf();
            while (leaf) |node| {
                var i: usize = 0;
                while (i < node.key_count) : (i += 1) {
                    const key = node.keys[i];
                    const rec = node.records[i];

                    // Write key length
                    const key_len: u16 = @intCast(key.data.len);
                    _ = index_file.write(std.mem.asBytes(&key_len)) catch return IsamError.IoError;

                    // Write key data
                    _ = index_file.write(key.data) catch return IsamError.IoError;

                    // Write RFA
                    const rfa_bytes = (RFA{ .block = rec.block, .offset = rec.offset }).toBytes();
                    _ = index_file.write(&rfa_bytes) catch return IsamError.IoError;
                }
                leaf = node.next_leaf;
            }

            _ = key_idx;
        }
    }

    /// Deserialize B-tree indexes from disk
    fn deserializeIndex(self: *Self) IsamError!void {
        const index_file = self.index_file orelse return IsamError.IoError;

        // Seek past header
        const header_size: u64 = DEFAULT_BLOCK_SIZE;
        index_file.seekTo(header_size) catch return IsamError.IoError;

        // For each B-tree, read and insert entries
        for (self.btrees) |*bt| {
            // Read entry count
            var count_buf: [4]u8 = undefined;
            const count_read = index_file.read(&count_buf) catch return IsamError.IoError;
            if (count_read < 4) {
                // No more data, this is OK for newly created files
                break;
            }
            const entry_count = std.mem.readInt(u32, &count_buf, .little);

            // Read each entry
            var i: u32 = 0;
            while (i < entry_count) : (i += 1) {
                // Read key length
                var len_buf: [2]u8 = undefined;
                _ = index_file.read(&len_buf) catch return IsamError.IoError;
                const key_len = std.mem.readInt(u16, &len_buf, .little);

                // Read key data
                const key_data = self.allocator.alloc(u8, key_len) catch return IsamError.OutOfMemory;
                _ = index_file.read(key_data) catch {
                    self.allocator.free(key_data);
                    return IsamError.IoError;
                };

                // Read RFA
                var rfa_buf: [8]u8 = undefined;
                _ = index_file.read(&rfa_buf) catch {
                    self.allocator.free(key_data);
                    return IsamError.IoError;
                };
                const rfa = RFA.fromBytes(rfa_buf);

                // Insert into B-tree
                const btree_key = btree.Key{ .data = key_data };
                const record_ptr = btree.RecordPtr{ .block = rfa.block, .offset = rfa.offset };
                bt.insert(btree_key, record_ptr) catch {
                    self.allocator.free(key_data);
                    return IsamError.OutOfMemory;
                };
            }
        }
    }

    // ============================================================
    // Private Implementation
    // ============================================================

    fn writeHeaders(self: *Self) IsamError!void {
        if (self.index_file) |f| {
            f.seekTo(0) catch return IsamError.IoError;
            _ = f.write(std.mem.asBytes(&self.index_header)) catch return IsamError.IoError;

            // Write key definitions after header
            for (self.key_defs) |key_def| {
                // Write number of segments
                const seg_count: u8 = @intCast(key_def.segments.len);
                _ = f.write(&[_]u8{seg_count}) catch return IsamError.IoError;

                // Write flags
                const flags: u8 = (@as(u8, if (key_def.allow_duplicates) 1 else 0)) |
                    (@as(u8, if (key_def.changes_allowed) 2 else 0));
                _ = f.write(&[_]u8{flags}) catch return IsamError.IoError;

                // Write key number
                _ = f.write(&[_]u8{key_def.key_number}) catch return IsamError.IoError;

                // Write each segment
                for (key_def.segments) |seg| {
                    _ = f.write(std.mem.asBytes(&seg.start)) catch return IsamError.IoError;
                    _ = f.write(std.mem.asBytes(&seg.length)) catch return IsamError.IoError;
                    _ = f.write(&[_]u8{@intFromEnum(seg.key_type)}) catch return IsamError.IoError;
                }
            }
        }
        if (self.data_file) |f| {
            f.seekTo(0) catch return IsamError.IoError;
            _ = f.write(std.mem.asBytes(&self.data_header)) catch return IsamError.IoError;
        }
    }

    fn readKeyDefs(self: *Self) IsamError!void {
        const f = self.index_file orelse return IsamError.IoError;

        // Seek past main header
        f.seekTo(@sizeOf(IndexHeader)) catch return IsamError.IoError;

        // Read key definitions
        const key_count = self.index_header.key_count;
        self.key_defs = self.allocator.alloc(KeyDef, key_count) catch return IsamError.OutOfMemory;

        for (self.key_defs, 0..) |*key_def, i| {
            // Read segment count
            var seg_count_buf: [1]u8 = undefined;
            _ = f.read(&seg_count_buf) catch return IsamError.IoError;
            const seg_count = seg_count_buf[0];

            // Read flags
            var flags_buf: [1]u8 = undefined;
            _ = f.read(&flags_buf) catch return IsamError.IoError;
            const flags = flags_buf[0];

            // Read key number
            var key_num_buf: [1]u8 = undefined;
            _ = f.read(&key_num_buf) catch return IsamError.IoError;

            // Allocate and read segments
            const segments = self.allocator.alloc(KeyDef.KeySegment, seg_count) catch return IsamError.OutOfMemory;

            for (segments) |*seg| {
                var start_buf: [4]u8 = undefined;
                _ = f.read(&start_buf) catch return IsamError.IoError;
                seg.start = std.mem.readInt(u32, &start_buf, .little);

                var len_buf: [4]u8 = undefined;
                _ = f.read(&len_buf) catch return IsamError.IoError;
                seg.length = std.mem.readInt(u32, &len_buf, .little);

                var type_buf: [1]u8 = undefined;
                _ = f.read(&type_buf) catch return IsamError.IoError;
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

    fn allocateRecord(self: *Self, size: usize) IsamError!RFA {
        _ = size;
        // TODO: Implement free list management
        // For now, append to end of file
        const data_file = self.data_file orelse return IsamError.IoError;
        const pos = data_file.getEndPos() catch return IsamError.IoError;

        return RFA{
            .block = @intCast(pos / self.index_header.block_size),
            .offset = @intCast(pos % self.index_header.block_size),
        };
    }

    fn writeRecord(self: *Self, rfa: RFA, record: []const u8) IsamError!void {
        const data_file = self.data_file orelse return IsamError.IoError;
        const pos = @as(u64, rfa.block) * self.index_header.block_size + rfa.offset;

        data_file.seekTo(pos) catch return IsamError.IoError;

        // Write record length (for variable length support)
        const len: u32 = @intCast(record.len);
        _ = data_file.write(std.mem.asBytes(&len)) catch return IsamError.IoError;

        // Write record data
        _ = data_file.write(record) catch return IsamError.IoError;
    }

    fn readRecord(self: *Self, rfa: RFA, buf: []u8) IsamError!usize {
        const data_file = self.data_file orelse return IsamError.IoError;
        const pos = @as(u64, rfa.block) * self.index_header.block_size + rfa.offset;

        data_file.seekTo(pos) catch return IsamError.IoError;

        // Read record length
        var len_buf: [4]u8 = undefined;
        _ = data_file.read(&len_buf) catch return IsamError.IoError;
        const len = std.mem.readInt(u32, &len_buf, .little);

        // Read record data
        const read_len = @min(len, buf.len);
        const bytes_read = data_file.read(buf[0..read_len]) catch return IsamError.IoError;

        return bytes_read;
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

/// Simple buffer pool for caching pages
pub const BufferPool = struct {
    allocator: std.mem.Allocator,
    pages: std.AutoHashMap(u64, Page),

    const Page = struct {
        data: []u8,
        dirty: bool,
    };

    pub fn init(allocator: std.mem.Allocator) BufferPool {
        return .{
            .allocator = allocator,
            .pages = std.AutoHashMap(u64, Page).init(allocator),
        };
    }

    pub fn deinit(self: *BufferPool) void {
        var it = self.pages.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.data);
        }
        self.pages.deinit();
    }

    pub fn flush(self: *BufferPool) IsamError!void {
        _ = self;
        // TODO: Write dirty pages to disk
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

    // Clean up test files
    std.fs.cwd().deleteFile("/tmp/test_isam.ism") catch {};
    std.fs.cwd().deleteFile("/tmp/test_isam.is1") catch {};
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

    // Clean up test files
    std.fs.cwd().deleteFile("/tmp/test_persist.ism") catch {};
    std.fs.cwd().deleteFile("/tmp/test_persist.is1") catch {};
}
