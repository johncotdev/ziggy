# Ziggy DBL Architecture

## System Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Source Code (.dbl)                          │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                            Lexer                                     │
│                    src/lexer/lexer.zig                              │
│         Converts source text to token stream                         │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                           Parser                                     │
│                   src/parser/parser.zig                             │
│         Builds Abstract Syntax Tree from tokens                      │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      Abstract Syntax Tree                            │
│                      src/ast/ast.zig                                │
│         In-memory representation of program structure                │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                    ┌──────────────┴──────────────┐
                    ▼                             ▼
┌─────────────────────────────┐   ┌─────────────────────────────────┐
│    Tree-Walking Interpreter │   │      Bytecode Compiler          │
│    src/runtime/runtime.zig  │   │   src/bytecode/compiler.zig     │
│    Direct AST execution     │   │   AST → Bytecode translation    │
└─────────────────────────────┘   └─────────────────────────────────┘
                    │                             │
                    │                             ▼
                    │             ┌─────────────────────────────────┐
                    │             │      Bytecode Module (.zbc)     │
                    │             │    src/bytecode/module.zig      │
                    │             │    Portable binary format       │
                    │             └─────────────────────────────────┘
                    │                             │
                    │                             ▼
                    │             ┌─────────────────────────────────┐
                    │             │      Virtual Machine            │
                    │             │      src/bytecode/vm.zig        │
                    │             │      Stack-based executor       │
                    │             └─────────────────────────────────┘
                    │                             │
                    └──────────────┬──────────────┘
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                         ISAM Database                                │
│                        src/isam/isam.zig                            │
│              B+ tree indexed file storage                            │
└─────────────────────────────────────────────────────────────────────┘
```

## Module Details

### Lexer (`src/lexer/`)

**Purpose**: Tokenize DBL source code into a stream of tokens.

**Key Files**:
- `lexer.zig` - Main tokenizer
- `token.zig` - Token type definitions

**Key Types**:
```zig
const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: u32,
    column: u32,
};
```

**DBL-Specific Features**:
- Case-insensitive keywords
- Semicolon and comma statement separators
- String literals with single/double quotes
- `%function` prefix for built-in functions

### Parser (`src/parser/`)

**Purpose**: Build AST from token stream using recursive descent parsing.

**Key Files**:
- `parser.zig` - Recursive descent parser

**Grammar Highlights**:
- `RECORD...ENDRECORD` for data structures
- `PROC` marks start of executable code
- `IF...THEN...ELSE...END` control flow
- `OPEN/READ/WRITE/CLOSE` for I/O

### AST (`src/ast/`)

**Purpose**: In-memory representation of parsed programs.

**Key File**: `ast.zig`

**Structure**:
```zig
pub const Program = struct {
    statements: []Statement,
    allocator: std.mem.Allocator,
};

pub const Statement = union(enum) {
    record: RecordDef,
    field: FieldDef,
    proc: ProcDef,
    assignment: Assignment,
    if_stmt: IfStatement,
    display_stmt: DisplayStatement,
    open_stmt: OpenStatement,
    // ... more statement types
};

pub const Expression = union(enum) {
    integer: i64,
    string: []const u8,
    identifier: []const u8,
    binary: *BinaryExpr,
    // ... more expression types
};
```

### Runtime Interpreter (`src/runtime/`)

**Purpose**: Direct execution of AST (tree-walking interpreter).

**Key File**: `runtime.zig`

**Features**:
- Channel-based I/O (SynergyDE compatible)
- ISAM file support
- XCALL external routine dispatch
- Terminal I/O via "tt:" device

**Execution Model**:
1. Walk the AST node by node
2. Evaluate expressions recursively
3. Execute statements sequentially
4. Handle I/O through channel abstraction

### Bytecode System (`src/bytecode/`)

**Purpose**: Compile AST to bytecode for portable, faster execution.

**Key Files**:
- `compiler.zig` - AST → Bytecode translator
- `vm.zig` - Stack-based bytecode interpreter
- `opcodes.zig` - Instruction definitions
- `module.zig` - Binary format specification
- `disasm.zig` - Disassembler for debugging

See [bytecode.md](./bytecode.md) for detailed documentation.

### ISAM Database (`src/isam/`)

**Purpose**: Indexed file storage with B+ tree indexes.

**Key Files**:
- `isam.zig` - High-level file operations
- `btree.zig` - B+ tree implementation

**Architecture**:
```
┌─────────────────┐     ┌─────────────────┐
│   .ism file     │     │   .is1 file     │
│   (index)       │     │   (data)        │
│                 │     │                 │
│  ┌───────────┐  │     │  ┌───────────┐  │
│  │  Header   │  │     │  │  Header   │  │
│  ├───────────┤  │     │  ├───────────┤  │
│  │  KeyDefs  │  │     │  │ Record 1  │  │
│  ├───────────┤  │     │  ├───────────┤  │
│  │  B+ Tree  │  │     │  │ Record 2  │  │
│  │   Nodes   │  │     │  ├───────────┤  │
│  └───────────┘  │     │  │   ...     │  │
└─────────────────┘     └─────────────────┘
```

See [isam.md](./isam.md) for detailed documentation.

## Data Flow Examples

### Running a DBL Program (Interpreter)

```
1. main.zig reads source file
2. Lexer tokenizes: "display(tt, 'Hello')" → [DISPLAY, LPAREN, IDENT, COMMA, STRING, RPAREN]
3. Parser builds AST: DisplayStatement { channel: "tt", expressions: ["Hello"] }
4. Runtime executes: Looks up channel tt, writes "Hello" to terminal
```

### Compiling to Bytecode

```
1. Parse source to AST
2. Compiler walks AST:
   - First pass: Collect record definitions → TypeDefs
   - Second pass: Collect field declarations → Globals
   - Third pass: Compile statements → Bytecode
3. Output Module with header, constants, types, routines, code
4. Serialize to .zbc file
```

### ISAM Store Operation

```
1. Runtime receives STORE(channel, record)
2. Lookup channel → IsamFile
3. Extract key from record using KeyDef segments
4. Insert key → RFA mapping into B+ tree
5. Append record data to .is1 file
6. Update B+ tree on disk
```

## Error Handling

The project uses Zig's error union pattern consistently:

```zig
pub const CompileError = error{
    OutOfMemory,
    TooManyConstants,
    UndefinedVariable,
    // ...
};

pub fn compile(self: *Self, program: *const ast.Program) CompileError!module.Module {
    // ...
}
```

Errors propagate up the call stack using `try` and are handled at appropriate boundaries.

## Memory Management

- All allocations use explicit allocators passed through the call chain
- The test allocator catches memory leaks in tests
- AST nodes own their child allocations
- Module/Compiler use ArrayList for dynamic collections
- ISAM B+ tree nodes are allocated and tracked for cleanup

## Threading Model

Currently single-threaded. Future considerations:
- ISAM record locking for multi-process access
- Potential for parallel compilation
- VM could support concurrent execution contexts
