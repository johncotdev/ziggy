//! B-Tree implementation for Ziggy ISAM
//!
//! Implements a B+ tree index structure for fast key-based lookups.

const std = @import("std");

/// B-tree order (max children per node)
pub const ORDER = 128;

/// Minimum keys per node (except root)
pub const MIN_KEYS = ORDER / 2 - 1;

/// Maximum keys per node
pub const MAX_KEYS = ORDER - 1;

/// B-tree node
pub const Node = struct {
    /// Is this a leaf node?
    is_leaf: bool,
    /// Number of keys currently in node
    key_count: u16,
    /// Keys stored in this node
    keys: [MAX_KEYS]Key,
    /// Child pointers (for internal nodes)
    children: [ORDER]?*Node,
    /// Record pointers (for leaf nodes)
    records: [MAX_KEYS]RecordPtr,
    /// Next leaf pointer (for sequential traversal)
    next_leaf: ?*Node,
    /// Parent pointer
    parent: ?*Node,

    const Self = @This();

    pub fn init(is_leaf: bool) Self {
        return .{
            .is_leaf = is_leaf,
            .key_count = 0,
            .keys = undefined,
            .children = [_]?*Node{null} ** ORDER,
            .records = undefined,
            .next_leaf = null,
            .parent = null,
        };
    }

    /// Check if node is full
    pub fn isFull(self: *const Self) bool {
        return self.key_count >= MAX_KEYS;
    }

    /// Check if node has minimum keys (for deletion)
    pub fn hasMinKeys(self: *const Self) bool {
        return self.key_count >= MIN_KEYS;
    }

    /// Find position for key in node
    pub fn findKeyPosition(self: *const Self, key: Key) usize {
        var low: usize = 0;
        var high: usize = self.key_count;

        while (low < high) {
            const mid = low + (high - low) / 2;
            if (self.keys[mid].compare(key) == .lt) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }

        return low;
    }

    /// Insert key at position (shifts existing keys)
    pub fn insertKeyAt(self: *Self, pos: usize, key: Key, record: RecordPtr) void {
        // Shift keys right
        var i: usize = self.key_count;
        while (i > pos) : (i -= 1) {
            self.keys[i] = self.keys[i - 1];
            self.records[i] = self.records[i - 1];
        }

        self.keys[pos] = key;
        self.records[pos] = record;
        self.key_count += 1;
    }

    /// Remove key at position
    pub fn removeKeyAt(self: *Self, pos: usize) void {
        // Shift keys left
        var i: usize = pos;
        while (i < self.key_count - 1) : (i += 1) {
            self.keys[i] = self.keys[i + 1];
            self.records[i] = self.records[i + 1];
        }

        self.key_count -= 1;
    }
};

/// Key structure
pub const Key = struct {
    data: []const u8,

    pub fn compare(self: Key, other: Key) std.math.Order {
        return std.mem.order(u8, self.data, other.data);
    }

    pub fn eql(self: Key, other: Key) bool {
        return std.mem.eql(u8, self.data, other.data);
    }

    pub fn startsWith(self: Key, prefix: Key) bool {
        if (prefix.data.len > self.data.len) return false;
        return std.mem.eql(u8, self.data[0..prefix.data.len], prefix.data);
    }
};

/// Record pointer (RFA)
pub const RecordPtr = struct {
    block: u48,
    offset: u16,
};

/// B-tree structure
pub const BTree = struct {
    allocator: std.mem.Allocator,
    root: ?*Node,
    height: usize,
    size: usize,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .root = null,
            .height = 0,
            .size = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        if (self.root) |root| {
            self.freeNode(root);
        }
    }

    fn freeNode(self: *Self, node: *Node) void {
        // Free key data
        var i: usize = 0;
        while (i < node.key_count) : (i += 1) {
            if (node.keys[i].data.len > 0) {
                self.allocator.free(@constCast(node.keys[i].data));
            }
        }

        if (!node.is_leaf) {
            for (node.children[0 .. node.key_count + 1]) |child| {
                if (child) |c| {
                    self.freeNode(c);
                }
            }
        }
        self.allocator.destroy(node);
    }

    /// Search for a key
    pub fn search(self: *Self, key: Key) ?RecordPtr {
        if (self.root == null) return null;

        var node = self.root.?;

        // Traverse to leaf
        while (!node.is_leaf) {
            const pos = node.findKeyPosition(key);
            node = node.children[pos] orelse return null;
        }

        // Search in leaf
        const pos = node.findKeyPosition(key);
        if (pos < node.key_count and node.keys[pos].eql(key)) {
            return node.records[pos];
        }

        return null;
    }

    /// Search for key with match mode
    pub fn searchWithMode(self: *Self, key: Key, mode: SearchMode) ?SearchResult {
        if (self.root == null) return null;

        var node = self.root.?;

        // Traverse to leaf
        while (!node.is_leaf) {
            const pos = node.findKeyPosition(key);
            node = node.children[pos] orelse return null;
        }

        const pos = node.findKeyPosition(key);

        switch (mode) {
            .exact => {
                if (pos < node.key_count and node.keys[pos].eql(key)) {
                    return SearchResult{ .node = node, .position = pos };
                }
                return null;
            },
            .greater_equal => {
                if (pos < node.key_count) {
                    return SearchResult{ .node = node, .position = pos };
                }
                // Check next leaf
                if (node.next_leaf) |next| {
                    return SearchResult{ .node = next, .position = 0 };
                }
                return null;
            },
            .greater => {
                var check_pos = pos;
                // Skip equal keys
                while (check_pos < node.key_count and node.keys[check_pos].eql(key)) {
                    check_pos += 1;
                }
                if (check_pos < node.key_count) {
                    return SearchResult{ .node = node, .position = check_pos };
                }
                if (node.next_leaf) |next| {
                    return SearchResult{ .node = next, .position = 0 };
                }
                return null;
            },
            .partial => {
                if (pos < node.key_count and node.keys[pos].startsWith(key)) {
                    return SearchResult{ .node = node, .position = pos };
                }
                return null;
            },
        }
    }

    pub const SearchMode = enum {
        exact,
        greater_equal,
        greater,
        partial,
    };

    pub const SearchResult = struct {
        node: *Node,
        position: usize,
    };

    /// Insert a key-record pair
    pub fn insert(self: *Self, key: Key, record: RecordPtr) !void {
        // Copy the key data to owned memory
        const owned_key_data = try self.allocator.dupe(u8, key.data);
        const owned_key = Key{ .data = owned_key_data };

        if (self.root == null) {
            // Create root leaf
            const root = try self.allocator.create(Node);
            root.* = Node.init(true);
            root.keys[0] = owned_key;
            root.records[0] = record;
            root.key_count = 1;
            self.root = root;
            self.height = 1;
            self.size = 1;
            return;
        }

        // Find leaf node
        var node = self.root.?;
        while (!node.is_leaf) {
            const pos = node.findKeyPosition(key);
            node = node.children[pos].?;
        }

        // Insert into leaf
        if (!node.isFull()) {
            const pos = node.findKeyPosition(key);
            node.insertKeyAt(pos, owned_key, record);
            self.size += 1;
        } else {
            // Node is full, need to split
            try self.insertAndSplit(node, owned_key, record);
            self.size += 1;
        }
    }

    fn insertAndSplit(self: *Self, node: *Node, key: Key, record: RecordPtr) !void {
        // Create new node
        const new_node = try self.allocator.create(Node);
        new_node.* = Node.init(node.is_leaf);

        // Determine split point
        const mid = (MAX_KEYS + 1) / 2;

        // Temporarily insert new key
        var all_keys: [MAX_KEYS + 1]Key = undefined;
        var all_records: [MAX_KEYS + 1]RecordPtr = undefined;

        const pos = node.findKeyPosition(key);

        // Copy keys before insertion point
        for (0..pos) |i| {
            all_keys[i] = node.keys[i];
            all_records[i] = node.records[i];
        }

        // Insert new key
        all_keys[pos] = key;
        all_records[pos] = record;

        // Copy remaining keys
        for (pos..node.key_count) |i| {
            all_keys[i + 1] = node.keys[i];
            all_records[i + 1] = node.records[i];
        }

        // Split: left node gets first half
        node.key_count = @intCast(mid);
        for (0..mid) |i| {
            node.keys[i] = all_keys[i];
            node.records[i] = all_records[i];
        }

        // Right node gets second half
        new_node.key_count = @intCast(MAX_KEYS + 1 - mid);
        for (mid..MAX_KEYS + 1) |i| {
            new_node.keys[i - mid] = all_keys[i];
            new_node.records[i - mid] = all_records[i];
        }

        // Link leaves
        if (node.is_leaf) {
            new_node.next_leaf = node.next_leaf;
            node.next_leaf = new_node;
        }

        // Promote middle key to parent
        const promoted_key = all_keys[mid];
        try self.insertIntoParent(node, promoted_key, new_node);
    }

    fn insertIntoParent(self: *Self, left: *Node, key: Key, right: *Node) !void {
        if (left.parent == null) {
            // Create new root
            const new_root = try self.allocator.create(Node);
            new_root.* = Node.init(false);
            new_root.keys[0] = key;
            new_root.children[0] = left;
            new_root.children[1] = right;
            new_root.key_count = 1;

            left.parent = new_root;
            right.parent = new_root;
            self.root = new_root;
            self.height += 1;
            return;
        }

        const parent = left.parent.?;

        if (!parent.isFull()) {
            // Insert into parent
            const pos = parent.findKeyPosition(key);

            // Shift children right
            var i: usize = parent.key_count + 1;
            while (i > pos + 1) : (i -= 1) {
                parent.children[i] = parent.children[i - 1];
            }
            parent.children[pos + 1] = right;
            right.parent = parent;

            // Shift keys right
            i = parent.key_count;
            while (i > pos) : (i -= 1) {
                parent.keys[i] = parent.keys[i - 1];
            }
            parent.keys[pos] = key;
            parent.key_count += 1;
        } else {
            // Split parent recursively
            // TODO: Implement parent split
        }
    }

    /// Delete a key
    pub fn delete(self: *Self, key: Key) bool {
        _ = self;
        _ = key;
        // TODO: Implement deletion with rebalancing
        return false;
    }

    /// Get first leaf for sequential traversal
    pub fn firstLeaf(self: *Self) ?*Node {
        if (self.root == null) return null;

        var node = self.root.?;
        while (!node.is_leaf) {
            node = node.children[0] orelse return null;
        }
        return node;
    }
};

test "btree basic operations" {
    const allocator = std.testing.allocator;
    var tree = BTree.init(allocator);
    defer tree.deinit();

    // Insert some keys
    const key1 = Key{ .data = "key001" };
    const record1 = RecordPtr{ .block = 1, .offset = 0 };
    try tree.insert(key1, record1);

    try std.testing.expectEqual(@as(usize, 1), tree.size);

    // Search for key
    const found = tree.search(key1);
    try std.testing.expect(found != null);
    try std.testing.expectEqual(@as(u48, 1), found.?.block);
}

test "btree key comparison" {
    const k1 = Key{ .data = "apple" };
    const k2 = Key{ .data = "banana" };
    const k3 = Key{ .data = "apple" };

    try std.testing.expectEqual(std.math.Order.lt, k1.compare(k2));
    try std.testing.expectEqual(std.math.Order.gt, k2.compare(k1));
    try std.testing.expect(k1.eql(k3));
}
