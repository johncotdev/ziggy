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
    current_key: u8,
    current_rfa: ?RFA,
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

        // Create files
        const index_file = std.fs.cwd().createFile(index_name, .{}) catch
            return IsamError.IoError;
        errdefer index_file.close();

        const data_file = std.fs.cwd().createFile(data_name, .{}) catch
            return IsamError.IoError;
        errdefer data_file.close();

        // Initialize headers
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
            .key_defs = allocator.dupe(KeyDef, key_defs) catch return IsamError.OutOfMemory,
            .current_key = 0,
            .current_rfa = null,
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

        self.* = .{
            .allocator = allocator,
            .index_file = index_file,
            .data_file = data_file,
            .index_header = index_header,
            .data_header = data_header,
            .key_defs = &[_]KeyDef{}, // TODO: Read from file
            .current_key = 0,
            .current_rfa = null,
            .is_locked = false,
            .buffer_pool = BufferPool.init(allocator),
        };

        return self;
    }

    pub const OpenMode = enum {
        read_only,
        read_write,
        exclusive,
    };

    /// Close the ISAM file
    pub fn close(self: *Self) void {
        self.buffer_pool.flush() catch {};
        self.buffer_pool.deinit();

        if (self.index_file) |*f| {
            f.close();
            self.index_file = null;
        }
        if (self.data_file) |*f| {
            f.close();
            self.data_file = null;
        }

        self.allocator.free(self.key_defs);
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

    /// Read next sequential record
    pub fn readNext(self: *Self, record_buf: []u8) IsamError!usize {
        if (self.current_rfa == null) {
            return IsamError.EndOfFile;
        }

        // Get next RFA from current position in index
        const next_rfa = try self.getNextRfa(self.current_key, self.current_rfa.?);

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
    }

    // ============================================================
    // Private Implementation
    // ============================================================

    fn writeHeaders(self: *Self) IsamError!void {
        if (self.index_file) |f| {
            f.seekTo(0) catch return IsamError.IoError;
            _ = f.write(std.mem.asBytes(&self.index_header)) catch return IsamError.IoError;
        }
        if (self.data_file) |f| {
            f.seekTo(0) catch return IsamError.IoError;
            _ = f.write(std.mem.asBytes(&self.data_header)) catch return IsamError.IoError;
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
        _ = self;
        _ = key_num;
        _ = key;
        _ = rfa;
        // TODO: B-tree insertion
    }

    fn findKey(self: *Self, key_num: u8, key: []const u8, match_mode: MatchMode) IsamError!RFA {
        _ = self;
        _ = key_num;
        _ = key;
        _ = match_mode;
        // TODO: B-tree search
        return IsamError.KeyNotFound;
    }

    fn getNextRfa(self: *Self, key_num: u8, current: RFA) IsamError!RFA {
        _ = self;
        _ = key_num;
        _ = current;
        // TODO: B-tree traversal
        return IsamError.EndOfFile;
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
