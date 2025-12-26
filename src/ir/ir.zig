//! Ziggy IR - Intermediate Representation
//!
//! This module defines the IR used between the AST and code generation backends.
//! The IR is designed to:
//! - Support multiple backends (bytecode VM, native via LLVM, Zig transpiler)
//! - Enable Zibol functions to be exported with C ABI for .NET interop
//! - Preserve type information for accurate code generation
//! - Be inspectable for debugging and optimization

const std = @import("std");
const Allocator = std.mem.Allocator;

// ============================================================================
// Types
// ============================================================================

/// Zibol type representation in IR
pub const Type = union(enum) {
    /// Void type (for procedures with no return)
    void: void,

    /// Alpha (character) type - ,a
    /// Value is the length in bytes
    alpha: u32,

    /// Decimal (implied decimal) type - ,d
    decimal: DecimalType,

    /// Integer type - ,i
    /// Value is the size in bytes (1, 2, 4, or 8)
    integer: u8,

    /// Packed decimal type - ,p
    packed_decimal: DecimalType,

    /// Pointer to another type
    ptr: *const Type,

    /// Array of a type
    array: struct {
        element: *const Type,
        length: u32,
    },

    /// Record (struct) type
    record: *const RecordType,

    /// Function type
    function: *const FunctionType,

    pub const DecimalType = struct {
        /// Total length in digits
        length: u32,
        /// Number of decimal places (implied)
        decimal_places: u8,
    };

    /// Get the size of this type in bytes
    pub fn sizeInBytes(self: Type) u32 {
        return switch (self) {
            .void => 0,
            .alpha => |len| len,
            .decimal => |d| d.length,
            .integer => |size| size,
            .packed_decimal => |d| (d.length + 2) / 2, // BCD packing
            .ptr => 8, // 64-bit pointers
            .array => |a| a.element.sizeInBytes() * a.length,
            .record => |r| r.size,
            .function => 8, // Function pointer
        };
    }

    /// Get the C ABI type name for this type
    pub fn cTypeName(self: Type) []const u8 {
        return switch (self) {
            .void => "void",
            .alpha => "uint8_t*", // Passed as pointer + length
            .decimal => "int64_t", // Implied decimal as integer
            .integer => |size| switch (size) {
                1 => "int8_t",
                2 => "int16_t",
                4 => "int32_t",
                8 => "int64_t",
                else => "int64_t",
            },
            .packed_decimal => "int64_t",
            .ptr => "void*",
            .array => "void*",
            .record => "void*",
            .function => "void*",
        };
    }
};

/// Record (structure) type definition
pub const RecordType = struct {
    name: []const u8,
    fields: []const Field,
    size: u32,

    pub const Field = struct {
        name: []const u8,
        ty: Type,
        offset: u32,
    };
};

/// Function type definition
pub const FunctionType = struct {
    params: []const Param,
    return_type: Type,
    is_variadic: bool,

    pub const Param = struct {
        name: []const u8,
        ty: Type,
        direction: ParamDirection,
    };

    pub const ParamDirection = enum {
        in, // Input only (read)
        out, // Output only (write)
        inout, // Both input and output
    };
};

// ============================================================================
// Values and Instructions
// ============================================================================

/// An SSA value reference
pub const Value = struct {
    id: u32,
    ty: Type,

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("%{d}", .{self.id});
    }
};

/// IR Instructions
pub const Instruction = union(enum) {
    // Memory operations
    alloca: Alloca,
    load: Load,
    store: Store,
    field_ptr: FieldPtr,

    // Arithmetic
    add: BinaryOp,
    sub: BinaryOp,
    mul: BinaryOp,
    div: BinaryOp,
    mod: BinaryOp,
    neg: UnaryOp,

    // Comparison
    cmp_eq: BinaryOp,
    cmp_ne: BinaryOp,
    cmp_lt: BinaryOp,
    cmp_le: BinaryOp,
    cmp_gt: BinaryOp,
    cmp_ge: BinaryOp,

    // Logical
    log_and: BinaryOp,
    log_or: BinaryOp,
    log_not: UnaryOp,

    // String operations
    str_concat: BinaryOp,
    str_compare: BinaryOp,
    str_copy: struct { dest: Value, src: Value, len: u32 },

    // Control flow
    br: Branch,
    cond_br: CondBranch,
    switch_br: Switch,
    ret: ?Value,

    // Function calls
    call: Call,
    xcall: XCall,

    // Type conversions
    alpha_to_decimal: struct { src: Value, decimal_places: u8 },
    decimal_to_alpha: struct { src: Value, width: u32 },
    int_to_decimal: UnaryOp,
    decimal_to_int: UnaryOp,

    // Constants
    const_int: struct { ty: Type, value: i64 },
    const_alpha: struct { value: []const u8 },
    const_decimal: struct { ty: Type, value: i64 },

    // I/O operations (lowered from OPEN, READ, WRITE, etc.)
    io_open: IoOpen,
    io_close: struct { channel: Value },
    io_read: IoRead,
    io_write: IoWrite,
    io_display: struct { channel: Value, values: []const Value },

    // Allocate instruction
    pub const Alloca = struct {
        ty: Type,
        name: []const u8,
        result: Value,
    };

    // Load from memory
    pub const Load = struct {
        ptr: Value,
        result: Value,
    };

    // Store to memory
    pub const Store = struct {
        ptr: Value,
        value: Value,
    };

    // Get pointer to record field
    pub const FieldPtr = struct {
        record_ptr: Value,
        field_index: u32,
        result: Value,
    };

    // Binary operation
    pub const BinaryOp = struct {
        lhs: Value,
        rhs: Value,
        result: Value,
    };

    // Unary operation
    pub const UnaryOp = struct {
        operand: Value,
        result: Value,
    };

    // Unconditional branch
    pub const Branch = struct {
        target: *Block,
    };

    // Conditional branch
    pub const CondBranch = struct {
        condition: Value,
        then_block: *Block,
        else_block: *Block,
    };

    // Switch/case branch
    pub const Switch = struct {
        value: Value,
        cases: []const Case,
        default: *Block,

        pub const Case = struct {
            value: i64,
            target: *Block,
        };
    };

    // Function call
    pub const Call = struct {
        callee: []const u8,
        args: []const Value,
        result: ?Value,
    };

    // External call (XCALL)
    pub const XCall = struct {
        routine: []const u8,
        args: []const Value,
    };

    // I/O operations
    pub const IoOpen = struct {
        channel: Value,
        mode: []const u8,
        filename: Value,
    };

    pub const IoRead = struct {
        channel: Value,
        record: Value,
        key: ?Value,
        qualifiers: IoQualifiers,
    };

    pub const IoWrite = struct {
        channel: Value,
        record: Value,
    };

    pub const IoQualifiers = struct {
        match_mode: MatchMode = .greater_equal,
        lock_mode: LockMode = .no_lock,
        key_number: u8 = 0,
        get_rfa: ?Value = null, // GETRFA qualifier
        use_rfa: bool = false, // RFA qualifier

        pub const MatchMode = enum { exact, greater_equal, greater, partial };
        pub const LockMode = enum { no_lock, shared, exclusive, auto_lock, manual };
    };
};

// ============================================================================
// Blocks and Functions
// ============================================================================

/// A basic block in the control flow graph
pub const Block = struct {
    /// Block label (for debugging/printing)
    label: []const u8,

    /// Instructions in this block
    instructions: std.ArrayListUnmanaged(Instruction),

    /// Predecessor blocks
    predecessors: std.ArrayListUnmanaged(*Block),

    /// Successor blocks (determined by terminator)
    successors: std.ArrayListUnmanaged(*Block),

    /// Owning function
    parent: *Function,

    /// Allocator for this block
    allocator: Allocator,

    pub fn init(allocator: Allocator, label: []const u8, parent: *Function) Block {
        return .{
            .label = label,
            .instructions = .{},
            .predecessors = .{},
            .successors = .{},
            .parent = parent,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Block) void {
        self.instructions.deinit(self.allocator);
        self.predecessors.deinit(self.allocator);
        self.successors.deinit(self.allocator);
    }

    pub fn append(self: *Block, inst: Instruction) !void {
        try self.instructions.append(self.allocator, inst);
    }

    pub fn isTerminated(self: *const Block) bool {
        if (self.instructions.items.len == 0) return false;
        const last = self.instructions.items[self.instructions.items.len - 1];
        return switch (last) {
            .br, .cond_br, .switch_br, .ret => true,
            else => false,
        };
    }
};

/// Linkage type for functions
pub const Linkage = enum {
    /// Internal function, not visible outside module
    internal,

    /// Exported with C ABI for external callers (.NET, etc.)
    export_c,

    /// Imported from external library
    external,
};

/// A function in the IR
pub const Function = struct {
    /// Function name
    name: []const u8,

    /// Export name (may differ from internal name for C ABI)
    export_name: ?[]const u8,

    /// Function signature
    signature: FunctionType,

    /// Linkage/visibility
    linkage: Linkage,

    /// Entry block
    entry: *Block,

    /// All blocks in the function
    blocks: std.ArrayListUnmanaged(*Block),

    /// Local variables (allocas)
    locals: std.ArrayListUnmanaged(Local),

    /// SSA value counter
    next_value_id: u32,

    /// Allocator for blocks and instructions
    allocator: Allocator,

    pub const Local = struct {
        name: []const u8,
        ty: Type,
        value: Value, // The alloca result
    };

    pub fn init(allocator: Allocator, name: []const u8, signature: FunctionType) !*Function {
        const self = try allocator.create(Function);
        self.* = .{
            .name = name,
            .export_name = null,
            .signature = signature,
            .linkage = .internal,
            .entry = undefined,
            .blocks = .{},
            .locals = .{},
            .next_value_id = 0,
            .allocator = allocator,
        };

        // Create entry block
        const entry = try allocator.create(Block);
        entry.* = Block.init(allocator, "entry", self);
        self.entry = entry;
        try self.blocks.append(allocator, entry);

        return self;
    }

    pub fn deinit(self: *Function) void {
        for (self.blocks.items) |block| {
            block.deinit();
            self.allocator.destroy(block);
        }
        self.blocks.deinit(self.allocator);
        self.locals.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    /// Create a new SSA value
    pub fn newValue(self: *Function, ty: Type) Value {
        const id = self.next_value_id;
        self.next_value_id += 1;
        return .{ .id = id, .ty = ty };
    }

    /// Create a new basic block
    pub fn createBlock(self: *Function, label: []const u8) !*Block {
        const block = try self.allocator.create(Block);
        block.* = Block.init(self.allocator, label, self);
        try self.blocks.append(self.allocator, block);
        return block;
    }

    /// Mark this function for export with C ABI
    pub fn setExport(self: *Function, export_name: []const u8) void {
        self.linkage = .export_c;
        self.export_name = export_name;
    }
};

// ============================================================================
// Module
// ============================================================================

/// An IR module (compilation unit)
pub const Module = struct {
    /// Module name
    name: []const u8,

    /// Library name for exports (if any)
    library_name: ?[]const u8,

    /// Global record definitions
    records: std.ArrayListUnmanaged(*RecordType),

    /// Global variables
    globals: std.ArrayListUnmanaged(Global),

    /// Functions
    functions: std.ArrayListUnmanaged(*Function),

    /// Allocator
    allocator: Allocator,

    pub const Global = struct {
        name: []const u8,
        ty: Type,
        initializer: ?[]const u8, // For initialized data
    };

    pub fn init(allocator: Allocator, name: []const u8) Module {
        return .{
            .name = name,
            .library_name = null,
            .records = .{},
            .globals = .{},
            .functions = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Module) void {
        for (self.functions.items) |func| {
            func.deinit();
        }
        self.functions.deinit(self.allocator);

        // Free record types and their fields
        for (self.records.items) |record| {
            self.allocator.free(record.fields);
            self.allocator.destroy(record);
        }
        self.records.deinit(self.allocator);

        self.globals.deinit(self.allocator);

        // Destroy the module itself
        self.allocator.destroy(self);
    }

    /// Get all exported functions
    pub fn getExports(self: *Module, allocator: Allocator) ![]*Function {
        var exports = std.ArrayListUnmanaged(*Function){};
        for (self.functions.items) |func| {
            if (func.linkage == .export_c) {
                try exports.append(allocator, func);
            }
        }
        return exports.toOwnedSlice(allocator);
    }

    /// Add a function to the module
    pub fn addFunction(self: *Module, func: *Function) !void {
        try self.functions.append(self.allocator, func);
    }

    /// Add a global variable
    pub fn addGlobal(self: *Module, name: []const u8, ty: Type) !void {
        try self.globals.append(self.allocator, .{
            .name = name,
            .ty = ty,
            .initializer = null,
        });
    }
};

// ============================================================================
// Tests
// ============================================================================

test "ir type sizes" {
    const alpha_type = Type{ .alpha = 10 };
    try std.testing.expectEqual(@as(u32, 10), alpha_type.sizeInBytes());

    const decimal_type = Type{ .decimal = .{ .length = 8, .decimal_places = 2 } };
    try std.testing.expectEqual(@as(u32, 8), decimal_type.sizeInBytes());

    const int_type = Type{ .integer = 4 };
    try std.testing.expectEqual(@as(u32, 4), int_type.sizeInBytes());
}

test "ir function creation" {
    const allocator = std.testing.allocator;

    const sig = FunctionType{
        .params = &[_]FunctionType.Param{
            .{ .name = "x", .ty = .{ .decimal = .{ .length = 8, .decimal_places = 2 } }, .direction = .in },
        },
        .return_type = .{ .decimal = .{ .length = 10, .decimal_places = 2 } },
        .is_variadic = false,
    };

    const func = try Function.init(allocator, "calculate_total", sig);
    defer func.deinit();

    func.setExport("ziggy_calculate_total");
    try std.testing.expectEqual(Linkage.export_c, func.linkage);
    try std.testing.expectEqualStrings("ziggy_calculate_total", func.export_name.?);
}

test "ir module with exports" {
    const allocator = std.testing.allocator;

    var module = Module.init(allocator, "pricing");
    defer module.deinit();

    module.library_name = "pricing";

    const sig = FunctionType{
        .params = &[_]FunctionType.Param{},
        .return_type = .void,
        .is_variadic = false,
    };

    const func = try Function.init(allocator, "my_func", sig);
    func.setExport("ziggy_my_func");
    try module.addFunction(func);

    try std.testing.expectEqual(@as(usize, 1), module.functions.items.len);
}
