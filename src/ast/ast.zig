//! Abstract Syntax Tree types for Ziggy DBL
//!
//! Represents the parsed structure of a DBL program.

const std = @import("std");
const Token = @import("../lexer/token.zig").Token;

/// A complete DBL program
pub const Program = struct {
    statements: []Statement,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{
            .statements = &[_]Statement{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        for (self.statements) |*stmt| {
            stmt.deinit(allocator);
        }
        allocator.free(self.statements);
    }
};

/// Statement types in DBL
pub const Statement = union(enum) {
    // Data division
    record: RecordDef,
    group: GroupDef,
    field: FieldDef,
    literal: LiteralDef,
    common: CommonDef,

    // Procedure division
    proc: ProcDef,
    assignment: Assignment,
    if_stmt: IfStatement,
    case_stmt: CaseStatement,
    loop: LoopStatement,
    call: CallStatement,
    xcall: XCallStatement,
    return_stmt: ReturnStatement,
    goto_stmt: GotoStatement,
    label: LabelDef,

    // I/O
    open_stmt: OpenStatement,
    close_stmt: CloseStatement,
    read_stmt: ReadStatement,
    write_stmt: WriteStatement,
    display_stmt: DisplayStatement,
    store_stmt: StoreStatement,
    delete_stmt: DeleteStatement,

    // Data manipulation
    clear_stmt: ClearStatement,
    init_stmt: InitStatement,
    incr_stmt: IncrStatement,

    // OOP
    class: ClassDef,
    method: MethodDef,
    namespace: NamespaceDef,

    // Expression statement
    expression: Expression,

    // Block
    block: Block,

    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .block => |*b| b.deinit(allocator),
            .if_stmt => |*i| i.deinit(allocator),
            .expression => |*e| e.deinit(allocator),
            .record => |r| allocator.free(r.fields),
            .group => |g| allocator.free(g.fields),
            .display_stmt => |d| allocator.free(d.expressions),
            .xcall => |x| allocator.free(x.arguments),
            else => {},
        }
    }
};

/// A block of statements
pub const Block = struct {
    statements: []Statement,

    pub fn deinit(self: *Block, allocator: std.mem.Allocator) void {
        for (self.statements) |*stmt| {
            stmt.deinit(allocator);
        }
        allocator.free(self.statements);
    }
};

/// Record definition
pub const RecordDef = struct {
    name: ?[]const u8,
    fields: []FieldDef,
};

/// Group definition (nested fields)
pub const GroupDef = struct {
    name: []const u8,
    fields: []FieldDef,
    overlay_target: ?[]const u8, // For overlays
};

/// Field definition
pub const FieldDef = struct {
    name: []const u8,
    data_type: DataType,
    initial_value: ?Expression,
    array_dims: ?[]usize,
};

/// DBL Data types
pub const DataType = union(enum) {
    alpha: AlphaType,
    decimal: DecimalType,
    implied_decimal: ImpliedDecimalType,
    integer: IntegerType,
    packed_decimal: PackedType,
    string: void,
    handle: void,
    structure_ref: []const u8, // @structname
    class_ref: []const u8, // @classname
};

pub const AlphaType = struct {
    size: ?usize, // null for a*, computed from initial value
};

pub const DecimalType = struct {
    size: ?usize, // null for d*, computed from initial value
};

pub const ImpliedDecimalType = struct {
    total_digits: usize,
    precision: usize,
};

pub const IntegerType = enum {
    i1,
    i2,
    i4,
    i8,
};

pub const PackedType = struct {
    size: usize,
    precision: ?usize,
};

/// Literal definition
pub const LiteralDef = struct {
    name: []const u8,
    data_type: DataType,
    value: Expression,
};

/// Common block definition
pub const CommonDef = struct {
    name: []const u8,
    fields: []FieldDef,
};

/// Procedure division marker
pub const ProcDef = struct {};

/// Assignment statement
pub const Assignment = struct {
    target: Expression,
    value: Expression,
};

/// If statement
pub const IfStatement = struct {
    condition: *Expression,
    then_branch: *Statement,
    else_branch: ?*Statement,

    pub fn deinit(self: *IfStatement, allocator: std.mem.Allocator) void {
        self.condition.deinit(allocator);
        allocator.destroy(self.condition);
        self.then_branch.deinit(allocator);
        allocator.destroy(self.then_branch);
        if (self.else_branch) |eb| {
            eb.deinit(allocator);
            allocator.destroy(eb);
        }
    }
};

/// Case statement
pub const CaseStatement = struct {
    selector: Expression,
    cases: []CaseBranch,
    default_branch: ?*Statement,
};

pub const CaseBranch = struct {
    values: []Expression,
    body: *Statement,
};

/// Loop statement (for, while, do-until, etc.)
pub const LoopStatement = struct {
    loop_type: LoopType,
    condition: ?Expression,
    init_expr: ?Expression,
    update_expr: ?Expression,
    body: *Statement,
};

pub const LoopType = enum {
    do_forever,
    do_until,
    while_loop,
    for_from_thru,
    for_do,
    foreach,
};

/// CALL statement (internal subroutine)
pub const CallStatement = struct {
    label: []const u8,
};

/// XCALL statement (external subroutine)
pub const XCallStatement = struct {
    routine_name: []const u8,
    arguments: []Expression,
};

/// Return statements
pub const ReturnStatement = struct {
    return_type: ReturnType,
    value: ?Expression,
};

pub const ReturnType = enum {
    xreturn,
    freturn,
    mreturn,
    simple_return,
};

/// GOTO statement
pub const GotoStatement = struct {
    label: []const u8,
};

/// Label definition
pub const LabelDef = struct {
    name: []const u8,
};

// I/O Statements
pub const OpenStatement = struct {
    channel: Expression,
    mode: []const u8,
    filename: Expression,
    qualifiers: []Qualifier,
};

pub const CloseStatement = struct {
    channel: Expression,
};

pub const ReadStatement = struct {
    channel: Expression,
    record: Expression,
    key: ?Expression,
    qualifiers: []Qualifier,
};

pub const WriteStatement = struct {
    channel: Expression,
    record: Expression,
    key: ?Expression,
    qualifiers: []Qualifier,
};

pub const DisplayStatement = struct {
    channel: Expression,
    expressions: []Expression,
};

pub const StoreStatement = struct {
    channel: Expression,
    record: Expression,
    qualifiers: []Qualifier,
};

pub const DeleteStatement = struct {
    channel: Expression,
};

pub const Qualifier = struct {
    name: []const u8,
    value: ?Expression,
};

// Data manipulation statements
pub const ClearStatement = struct {
    target: Expression,
};

pub const InitStatement = struct {
    target: Expression,
};

pub const IncrStatement = struct {
    target: Expression,
    amount: ?Expression,
};

// OOP
pub const ClassDef = struct {
    name: []const u8,
    extends: ?[]const u8,
    implements: [][]const u8,
    members: []ClassMember,
};

pub const ClassMember = union(enum) {
    field: FieldDef,
    method: MethodDef,
    property: PropertyDef,
};

pub const MethodDef = struct {
    name: []const u8,
    access: AccessModifier,
    is_static: bool,
    return_type: ?DataType,
    parameters: []ParameterDef,
    body: []Statement,
};

pub const PropertyDef = struct {
    name: []const u8,
    access: AccessModifier,
    data_type: DataType,
    getter: ?[]Statement,
    setter: ?[]Statement,
};

pub const ParameterDef = struct {
    name: []const u8,
    data_type: DataType,
};

pub const AccessModifier = enum {
    public,
    private,
    protected,
    internal,
};

pub const NamespaceDef = struct {
    name: []const u8,
    members: []Statement,
};

/// Expression types
pub const Expression = union(enum) {
    // Literals
    integer: i64,
    decimal: []const u8, // Store as string to preserve precision
    string: []const u8,
    identifier: []const u8,
    null_literal: void,

    // Compound
    binary: *BinaryExpr,
    unary: *UnaryExpr,
    call: *CallExpr,
    index: *IndexExpr,
    member: *MemberExpr,
    range: *RangeExpr,

    // Special
    grouping: *Expression,

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binary => |b| {
                b.left.deinit(allocator);
                b.right.deinit(allocator);
                allocator.destroy(b);
            },
            .unary => |u| {
                u.operand.deinit(allocator);
                allocator.destroy(u);
            },
            .grouping => |g| {
                g.deinit(allocator);
                allocator.destroy(g);
            },
            else => {},
        }
    }
};

pub const BinaryExpr = struct {
    left: Expression,
    operator: BinaryOp,
    right: Expression,
};

pub const BinaryOp = enum {
    // Arithmetic
    add,
    subtract,
    multiply,
    divide,
    int_divide,
    modulo,
    power,

    // Comparison
    equal,
    not_equal,
    less_than,
    less_equal,
    greater_than,
    greater_equal,

    // Logical
    logical_and,
    logical_or,
    logical_xor,

    // String
    concat,
};

pub const UnaryExpr = struct {
    operator: UnaryOp,
    operand: Expression,
};

pub const UnaryOp = enum {
    negate,
    logical_not,
    bitwise_not,
};

pub const CallExpr = struct {
    callee: []const u8,
    arguments: []Expression,
    is_function: bool, // %function vs subroutine
};

pub const IndexExpr = struct {
    base: Expression,
    indices: []Expression,
};

pub const MemberExpr = struct {
    object: Expression,
    member: []const u8,
};

pub const RangeExpr = struct {
    base: Expression,
    start: Expression,
    end_or_length: Expression,
    is_length: bool, // true for (start,length) vs false for (start:end)
};

test "ast basic structure" {
    const allocator = std.testing.allocator;
    var program = Program.init(allocator);
    defer program.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), program.statements.len);
}
