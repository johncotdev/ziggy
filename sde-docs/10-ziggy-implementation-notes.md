# Ziggy DBL Implementation Notes

This document outlines considerations for implementing DBL in Zig as the "Ziggy DBL" project.

## Project Goals

1. **Learn Zig** - Build practical experience with systems programming in Zig
2. **Learn Compilers** - Understand lexing, parsing, AST, code generation
3. **Feature Parity with DBL** - Eventually compile and run DBL programs
4. **Custom ISAM** - Replace proprietary ISAM with open-source implementation
5. **Open Source** - Personal development, no commercial constraints

## Comparison: Bun/Node Analogy

The user mentioned "Similar to how Bun has progressed as a Rust implementation of Node.js" - note that Bun is actually written in **Zig**, not Rust! This is a great precedent:

| Node.js | Bun | SynergyDE | Ziggy DBL |
|---------|-----|-----------|-----------|
| V8 Engine | Zig + JavaScriptCore | Synergy Runtime | Zig Runtime |
| C++ | Zig | Proprietary | Zig |
| npm | bun install | Repository | TBD |
| node_modules | bun ecosystem | Synergy DBMS | Ziggy ISAM |

## Implementation Phases

### Phase 1: Lexer
Convert source code into tokens.

**DBL Token Categories:**
- Keywords: `RECORD`, `ENDRECORD`, `PROC`, `END`, `IF`, `THEN`, `ELSE`, etc.
- Identifiers: Variable names, routine names
- Literals: Alpha `"string"`, Decimal `123`, Implied `123.45`
- Operators: `+`, `-`, `*`, `/`, `.EQ.`, `.AND.`, etc.
- Delimiters: `,`, `(`, `)`, `[`, `]`
- Comments: `;` to end of line

**Zig Considerations:**
- Use Zig's comptime for token definitions
- Leverage Zig's tagged unions for token types
- Memory-efficient string handling

### Phase 2: Parser
Build Abstract Syntax Tree from tokens.

**DBL Grammar Highlights:**
- Program = DataDivision + PROC + ProcedureDivision + END
- Statements are line-based (no explicit terminators)
- Case-insensitive keywords
- Group/Record nesting

**Zig Considerations:**
- Use allocator-aware AST nodes
- Consider arena allocators for parse trees
- Error recovery strategies

### Phase 3: Semantic Analysis
Type checking, scope resolution.

**DBL Specifics:**
- Alpha, Decimal, Implied-Decimal type system
- Implicit coercion rules
- Record/Group field resolution
- Common block linking

### Phase 4: Code Generation

**Options:**
1. **Interpreter** - Direct AST walking (simplest to start)
2. **Bytecode VM** - Compile to intermediate bytecode
3. **Native Code** - Generate machine code via LLVM or Zig
4. **Transpiler** - Generate Zig code, compile with Zig

**Recommendation:** Start with interpreter, evolve to bytecode VM.

### Phase 5: Runtime System
Execution environment providing:
- Memory management
- File I/O
- Terminal I/O
- Built-in functions
- Error handling

### Phase 6: ISAM Implementation
Custom indexed file system.

## Ziggy ISAM Design

### Core Components

```
┌─────────────────────────────────────────────────────────┐
│                    Ziggy ISAM                           │
├─────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │
│  │  B-Tree     │  │   Record    │  │   Lock          │  │
│  │  Index      │  │   Store     │  │   Manager       │  │
│  └─────────────┘  └─────────────┘  └─────────────────┘  │
├─────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │
│  │  Buffer     │  │   Free      │  │   Recovery      │  │
│  │  Pool       │  │   Space     │  │   Log           │  │
│  └─────────────┘  └─────────────┘  └─────────────────┘  │
├─────────────────────────────────────────────────────────┤
│                     File I/O Layer                       │
└─────────────────────────────────────────────────────────┘
```

### B-Tree Index

```zig
const BTreeNode = struct {
    is_leaf: bool,
    key_count: u16,
    keys: [MAX_KEYS]Key,
    children: [MAX_KEYS + 1]?*BTreeNode,  // Non-leaf only
    record_ptrs: [MAX_KEYS]RecordPtr,      // Leaf only
};

const Key = struct {
    data: []const u8,
    key_type: KeyType,
};

const KeyType = enum {
    alpha,
    decimal,
    integer,
    packed,
    nocase,
};
```

### Record Storage

```zig
const RecordStore = struct {
    data_file: std.fs.File,
    record_size: ?u32,  // null for variable length
    free_list: FreeList,

    pub fn store(self: *RecordStore, record: []const u8) !RecordPtr {
        // Find space, write record, return pointer
    }

    pub fn read(self: *RecordStore, ptr: RecordPtr) ![]u8 {
        // Read record at pointer
    }

    pub fn delete(self: *RecordStore, ptr: RecordPtr) !void {
        // Mark space as free
    }
};
```

### Lock Manager

```zig
const LockManager = struct {
    locks: std.AutoHashMap(RecordPtr, LockInfo),
    mutex: std.Thread.Mutex,

    const LockInfo = struct {
        owner: ThreadId,
        mode: LockMode,
        timestamp: i64,
    };

    const LockMode = enum {
        no_lock,
        shared,
        exclusive,
    };

    pub fn acquire(self: *LockManager, ptr: RecordPtr, mode: LockMode, timeout: ?i64) !void {
        // Acquire lock with optional timeout
    }

    pub fn release(self: *LockManager, ptr: RecordPtr) void {
        // Release lock
    }
};
```

### Buffer Pool

```zig
const BufferPool = struct {
    pages: []Page,
    page_table: std.AutoHashMap(PageId, usize),
    clock_hand: usize,  // For clock replacement algorithm

    pub fn getPage(self: *BufferPool, page_id: PageId) !*Page {
        // Return cached page or load from disk
    }

    pub fn markDirty(self: *BufferPool, page_id: PageId) void {
        // Mark page as needing write-back
    }

    pub fn flush(self: *BufferPool) !void {
        // Write all dirty pages to disk
    }
};
```

## Data Type Mapping

| DBL Type | Zig Type | Storage |
|----------|----------|---------|
| `a` (alpha) | `[]u8` | ASCII bytes |
| `d` (decimal) | `[]u8` | ASCII digits |
| `d.` (implied) | Custom struct | ASCII + precision |
| `i1` | `i8` | 1 byte |
| `i2` | `i16` | 2 bytes |
| `i4` | `i32` | 4 bytes |
| `i8` | `i64` | 8 bytes |
| `p` (packed) | Custom struct | BCD + sign |
| `string` | `[]const u8` or `std.ArrayList` | Dynamic |

### Decimal Implementation

```zig
const Decimal = struct {
    digits: [28]u8,  // ASCII '0'-'9'
    sign: Sign,
    length: u8,

    const Sign = enum { positive, negative };

    pub fn add(a: Decimal, b: Decimal) Decimal { ... }
    pub fn multiply(a: Decimal, b: Decimal) Decimal { ... }
    pub fn compare(a: Decimal, b: Decimal) std.math.Order { ... }
};

const ImpliedDecimal = struct {
    value: Decimal,
    precision: u8,  // Digits after decimal point
};
```

## File Format

### Index File Header
```
┌────────────────────────────────────────────────────────┐
│ Magic Number (8 bytes): "ZIGGYIDX"                     │
│ Version (4 bytes)                                      │
│ Block Size (4 bytes)                                   │
│ Key Count (2 bytes)                                    │
│ Key Definitions (variable)                             │
│ Root Block Pointer (8 bytes)                           │
│ Record Count (8 bytes)                                 │
│ Flags (4 bytes)                                        │
└────────────────────────────────────────────────────────┘
```

### Data File Header
```
┌────────────────────────────────────────────────────────┐
│ Magic Number (8 bytes): "ZIGGYDAT"                     │
│ Version (4 bytes)                                      │
│ Record Type (1 byte): Fixed/Variable/Multi             │
│ Record Size (4 bytes): For fixed-length                │
│ Free List Head (8 bytes)                               │
│ Record Count (8 bytes)                                 │
└────────────────────────────────────────────────────────┘
```

## Testing Strategy

1. **Unit Tests** - Test each component in isolation
2. **Integration Tests** - Test component interaction
3. **Compatibility Tests** - Compare output with Synergy DBL
4. **Performance Tests** - Benchmark against Synergy ISAM
5. **Stress Tests** - Concurrent access, large files

## Development Milestones

### Milestone 1: Hello World
- Basic lexer and parser
- Simple DISPLAY statement
- Interpreter execution

### Milestone 2: Variables
- RECORD definitions
- Variable assignment
- Basic expressions

### Milestone 3: Control Flow
- IF/THEN/ELSE
- CASE/ENDCASE
- Loops (FOR, WHILE, DO)

### Milestone 4: Subroutines
- SUBROUTINE/FUNCTION definitions
- XCALL/FRETURN
- Parameter passing

### Milestone 5: Basic File I/O
- Sequential files (OPEN, READS, WRITES)
- Text file processing

### Milestone 6: ISAM
- B-tree index implementation
- STORE, READ, WRITE, DELETE
- Basic locking

### Milestone 7: OOP
- CLASS/ENDCLASS
- Properties and Methods
- Inheritance

## Resources

### Zig Resources
- [Zig Language Reference](https://ziglang.org/documentation/master/)
- [Zig Standard Library](https://ziglang.org/documentation/master/std/)
- [Zig Learn](https://ziglearn.org/)

### Compiler Resources
- "Crafting Interpreters" by Robert Nystrom
- "Engineering a Compiler" by Cooper & Torczon
- "Modern Compiler Implementation" by Appel

### Database Resources
- "Database Internals" by Alex Petrov
- "Designing Data-Intensive Applications" by Martin Kleppmann
- SQLite source code (excellent B-tree reference)

### Synergy Resources
- [Synergex Documentation](https://synergex.com/docs/)
- [DBL Language Reference](https://www.synergex.com/docs/lrm/)
