# Ziggy DBL Abstract Syntax Tree

## Overview

The AST represents the parsed structure of a DBL program. It serves as the intermediate representation between parsing and execution/compilation.

**Location**: `src/ast/ast.zig`

## Program Structure

```zig
pub const Program = struct {
    statements: []Statement,  // All top-level statements
    allocator: std.mem.Allocator,
};
```

A DBL program is a flat sequence of statements. The structure is:
1. Data declarations (records, fields, common blocks)
2. `PROC` marker
3. Executable statements

## Statement Types

```zig
pub const Statement = union(enum) {
    // Data Division
    record: RecordDef,
    group: GroupDef,
    field: FieldDef,
    literal: LiteralDef,
    common: CommonDef,

    // Procedure Division
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

    // I/O Statements
    open_stmt: OpenStatement,
    close_stmt: CloseStatement,
    read_stmt: ReadStatement,
    write_stmt: WriteStatement,
    display_stmt: DisplayStatement,
    store_stmt: StoreStatement,
    delete_stmt: DeleteStatement,

    // Data Manipulation
    clear_stmt: ClearStatement,
    init_stmt: InitStatement,
    incr_stmt: IncrStatement,

    // OOP
    class: ClassDef,
    method: MethodDef,
    namespace: NamespaceDef,

    // Other
    expression: Expression,
    block: Block,
};
```

## Data Definitions

### Records

```zig
pub const RecordDef = struct {
    name: ?[]const u8,    // null for unnamed records
    fields: []FieldDef,
};

// Example DBL:
// record customer
//     id      ,a8
//     name    ,a30
// endrecord
```

### Fields

```zig
pub const FieldDef = struct {
    name: []const u8,
    data_type: DataType,
    initial_value: ?Expression,
    array_dims: ?[]usize,
};
```

### Data Types

```zig
pub const DataType = union(enum) {
    alpha: AlphaType,           // aNN or a*
    decimal: DecimalType,       // dNN or d*
    implied_decimal: ImpliedDecimalType,  // dNN.PP
    integer: IntegerType,       // i1, i2, i4, i8
    packed_decimal: PackedType,
    string: void,
    handle: void,
    structure_ref: []const u8,  // @structname
    class_ref: []const u8,      // @classname
};

pub const AlphaType = struct {
    size: ?usize,  // null for a*
};

pub const DecimalType = struct {
    size: ?usize,  // null for d*
};

pub const ImpliedDecimalType = struct {
    total_digits: usize,
    precision: usize,
};

pub const IntegerType = enum { i1, i2, i4, i8 };
```

## Control Flow

### If Statement

```zig
pub const IfStatement = struct {
    condition: *Expression,
    then_branch: *Statement,
    else_branch: ?*Statement,
};

// Example DBL:
// if (x > 0)
//     display(tt, "positive")
// else
//     display(tt, "non-positive")
```

### Case Statement (USING)

```zig
pub const CaseStatement = struct {
    selector: Expression,
    cases: []CaseBranch,
    default_branch: ?*Statement,
};

pub const CaseBranch = struct {
    values: []Expression,
    body: *Statement,
};

// Example DBL:
// using x select
// (1), display(tt, "one")
// (2), display(tt, "two")
// (), display(tt, "other")
// endusing
```

### Loops

```zig
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

// Example DBL:
// for i from 1 thru 10
//     display(tt, i)
```

## I/O Statements

### Open

```zig
pub const OpenStatement = struct {
    channel: Expression,
    mode: []const u8,      // "I", "O", "U", "U:I", etc.
    filename: Expression,
    qualifiers: []Qualifier,
};

pub const Qualifier = struct {
    name: []const u8,
    value: ?Expression,
};

// Example DBL:
// open(ch, "U:I", "customers")
```

### Display

```zig
pub const DisplayStatement = struct {
    channel: Expression,
    expressions: []Expression,
};

// Example DBL:
// display(tt, "Name: ", customer.name)
```

### Read/Write

```zig
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
```

## Expressions

```zig
pub const Expression = union(enum) {
    // Literals
    integer: i64,
    decimal: []const u8,  // String to preserve precision
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

    // Grouping
    grouping: *Expression,
};
```

### Binary Expressions

```zig
pub const BinaryExpr = struct {
    left: Expression,
    operator: BinaryOp,
    right: Expression,
};

pub const BinaryOp = enum {
    // Arithmetic
    add, subtract, multiply, divide, int_divide, modulo, power,

    // Comparison
    equal, not_equal, less_than, less_equal, greater_than, greater_equal,

    // Logical
    logical_and, logical_or, logical_xor,

    // String
    concat,
};
```

### Unary Expressions

```zig
pub const UnaryExpr = struct {
    operator: UnaryOp,
    operand: Expression,
};

pub const UnaryOp = enum {
    negate,       // -x
    logical_not,  // .not. x
    bitwise_not,  // ^x
};
```

### Function Calls

```zig
pub const CallExpr = struct {
    callee: []const u8,
    arguments: []Expression,
    is_function: bool,  // %function vs subroutine
};

// Example DBL:
// result = %trim(input)
```

### Member Access

```zig
pub const MemberExpr = struct {
    object: Expression,
    member: []const u8,
};

// Example DBL:
// customer.name
```

### Index/Array Access

```zig
pub const IndexExpr = struct {
    base: Expression,
    indices: []Expression,
};

// Example DBL:
// array[i]
// matrix[row, col]
```

### Range/Substring Access

```zig
pub const RangeExpr = struct {
    base: Expression,
    start: Expression,
    end_or_length: Expression,
    is_length: bool,  // (start,length) vs (start:end)
};

// Example DBL:
// name(1:10)    ; Characters 1-10
// name(5,3)     ; 3 characters starting at 5
```

## OOP Constructs

```zig
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

pub const AccessModifier = enum {
    public, private, protected, internal,
};
```

## Memory Management

AST nodes that contain heap-allocated children implement `deinit`:

```zig
pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
    switch (self.*) {
        .block => |*b| b.deinit(allocator),
        .if_stmt => |*i| i.deinit(allocator),
        .expression => |*e| e.deinit(allocator),
        .record => |r| allocator.free(r.fields),
        // ...
    }
}
```

Pointer fields (like `*Expression` in `BinaryExpr`) must be properly destroyed:

```zig
pub fn deinit(self: *Expression, allocator: Allocator) void {
    switch (self.*) {
        .binary => |b| {
            b.left.deinit(allocator);
            b.right.deinit(allocator);
            allocator.destroy(b);
        },
        // ...
    }
}
```

## Usage in Compiler

The bytecode compiler walks the AST to generate code:

```zig
fn compileStatement(self: *Compiler, stmt: *const ast.Statement) !void {
    switch (stmt.*) {
        .assignment => |assign| try self.compileAssignment(&assign),
        .if_stmt => |if_s| try self.compileIf(&if_s),
        .display_stmt => |display| try self.compileDisplay(&display),
        // ...
    }
}

fn compileExpression(self: *Compiler, expr: *const ast.Expression) !void {
    switch (expr.*) {
        .integer => |val| try self.compileIntLiteral(val),
        .binary => |bin| try self.compileBinaryOp(bin),
        .identifier => |name| try self.emitLoadVar(name),
        // ...
    }
}
```

## Usage in Runtime

The tree-walking interpreter also traverses the AST:

```zig
fn executeStatement(self: *Runtime, stmt: Statement) !void {
    switch (stmt) {
        .assignment => |assign| try self.executeAssignment(assign),
        .if_stmt => |if_s| try self.executeIf(if_s),
        .display_stmt => |display| try self.executeDisplay(display),
        // ...
    }
}

fn evaluateExpression(self: *Runtime, expr: Expression) !Value {
    switch (expr) {
        .integer => |val| return Value{ .integer = val },
        .binary => |bin| return try self.evaluateBinary(bin),
        .identifier => |name| return try self.lookupVariable(name),
        // ...
    }
}
```
