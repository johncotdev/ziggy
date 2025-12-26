# Ziggy DBL Bytecode System

## Overview

The bytecode system provides an alternative execution path to the tree-walking interpreter. It compiles DBL source code to a portable binary format that can be executed by a stack-based virtual machine.

```
Source (.dbl) → Lexer → Parser → AST → Compiler → Module (.zbc) → VM → Execution
```

## Components

### 1. Compiler (`src/bytecode/compiler.zig`)

Transforms AST into bytecode using a multi-pass approach:

**Pass 1: Collect Declarations**
- Record definitions → TypeDefs in module
- Creates field-to-offset mappings

**Pass 2: Collect Globals**
- Field declarations → Global variable slots
- Common blocks → Shared global slots

**Pass 3: Code Generation**
- Statements after `proc` → Bytecode instructions
- Expressions → Stack-based evaluation code

#### Key Data Structures

```zig
pub const Compiler = struct {
    allocator: Allocator,

    // Output sections
    code: std.ArrayList(u8),           // Bytecode instructions
    constants: std.ArrayList(Constant), // Constant pool
    types: std.ArrayList(TypeDef),      // Record definitions
    routines: std.ArrayList(RoutineDef),// Subroutine metadata

    // Symbol tables
    globals: std.StringHashMap(VarInfo),  // Global variables
    locals: std.StringHashMap(VarInfo),   // Current routine locals
    labels: std.StringHashMap(Label),     // Jump targets

    // Deduplication
    string_constants: std.StringHashMap(u16), // String → index
    int_constants: std.AutoHashMap(i64, u16), // Int → index

    // Stack tracking
    max_stack: u16,
    current_stack: u16,
};
```

#### Compilation Process

```zig
pub fn compile(self: *Self, program: *const ast.Program) CompileError!module.Module {
    // Pass 1: Records
    for (program.statements) |stmt| {
        if (stmt == .record) try self.compileRecordDef(&stmt.record);
    }

    // Pass 2: Fields/Globals
    for (program.statements) |stmt| {
        if (stmt == .field) try self.addGlobal(stmt.field.name, ...);
    }

    // Pass 3: Code
    var in_proc = false;
    for (program.statements) |stmt| {
        if (stmt == .proc) in_proc = true;
        else if (in_proc) try self.compileStatement(&stmt);
    }

    try self.emit(.halt);
    try self.resolveLabels();
    return self.buildModule();
}
```

### 2. Opcodes (`src/bytecode/opcodes.zig`)

Defines 100+ instructions organized by category:

| Range | Category | Examples |
|-------|----------|----------|
| 0x00-0x0F | Stack | `push_i8`, `pop`, `dup`, `swap` |
| 0x10-0x1F | Locals | `load_local_0`, `store_local` |
| 0x20-0x2F | Globals | `load_global`, `store_global` |
| 0x30-0x3F | Records | `new_record`, `load_field` |
| 0x40-0x4F | Arithmetic | `add`, `sub`, `mul`, `div` |
| 0x50-0x5F | Comparison | `cmp_eq`, `cmp_lt`, `log_and` |
| 0x60-0x6F | Control | `jump`, `jump_if_false`, `call` |
| 0x70-0x7F | Calls | `call`, `ret`, `xcall` |
| 0x80-0x8F | Channel I/O | `ch_open`, `ch_display` |
| 0x90-0x9F | ISAM | `isam_read`, `isam_store` |
| 0xA0-0xAF | Strings | `str_concat`, `str_slice` |
| 0xB0-0xBF | Conversion | `to_int`, `to_str` |
| 0xC0-0xCF | Built-ins | `fn_abs`, `fn_date` |
| 0xF0-0xFF | Extended | `halt`, `debug_break` |

#### Operand Sizes

```zig
pub fn operandSize(self: Opcode) usize {
    return switch (self) {
        .nop, .pop, .add, .ret => 0,        // No operands
        .push_i8, .load_local => 1,          // 1-byte operand
        .push_i16, .load_global, .call => 2, // 2-byte operand
        .push_i32, .jump_wide => 4,          // 4-byte operand
        .push_i64 => 8,                      // 8-byte operand
        .xcall => 3,                         // [u16 name, u8 argc]
        // ...
    };
}
```

### 3. Module Format (`src/bytecode/module.zig`)

Binary file format for compiled modules:

```
┌────────────────────────────────────────┐
│              Header (32 bytes)          │
│  Magic: "ZBC1"                          │
│  Version: major.minor                   │
│  Flags: debug, native, isam, library    │
│  Section count, entry point, hash       │
├────────────────────────────────────────┤
│           Section Table                 │
│  [type, offset, size, entry_count]...   │
├────────────────────────────────────────┤
│           Constants Section             │
│  [tag, data]...                         │
│  Tags: integer, decimal, string, etc.   │
├────────────────────────────────────────┤
│            Types Section                │
│  [TypeDef with FieldDefs]...            │
├────────────────────────────────────────┤
│           Routines Section              │
│  [name, flags, code_offset, params]...  │
├────────────────────────────────────────┤
│             Code Section                │
│  Raw bytecode bytes                     │
├────────────────────────────────────────┤
│          Debug Section (optional)       │
│  Line table, local variable info        │
└────────────────────────────────────────┘
```

#### Key Types

```zig
pub const ModuleHeader = extern struct {
    magic: [4]u8,           // "ZBC1"
    version_major: u16,     // 0
    version_minor: u16,     // 1
    flags: ModuleFlags,
    section_count: u32,
    entry_point: u32,       // 0xFFFFFFFF if library
    source_hash: u32,
    _reserved: [8]u8,
};

pub const Constant = union(ConstantTag) {
    integer: i64,
    decimal: struct { value: i64, precision: u8 },
    string: []const u8,
    alpha: struct { data: []const u8, size: u16 },
    identifier: []const u8,
    record_ref: u16,
    routine_ref: u16,
};
```

### 4. Virtual Machine (`src/bytecode/vm.zig`)

Stack-based bytecode interpreter:

```zig
pub const VM = struct {
    allocator: Allocator,
    module: *const Module,

    // Execution state
    ip: u32,                    // Instruction pointer
    stack: [STACK_SIZE]Value,   // Operand stack
    sp: u32,                    // Stack pointer

    // Variable storage
    globals: []Value,
    locals: []Value,

    // Call stack
    frames: [MAX_FRAMES]CallFrame,
    frame_count: u32,

    // I/O
    channels: [MAX_CHANNELS]?*Channel,
};
```

#### Execution Loop

```zig
pub fn run(self: *Self) VMError!void {
    while (true) {
        const op = self.readOpcode();
        switch (op) {
            .push_i8 => {
                const val = self.readByte();
                try self.push(.{ .integer = val });
            },
            .add => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(.{ .integer = a.integer + b.integer });
            },
            .jump_if_false => {
                const offset = self.readI16();
                const cond = try self.pop();
                if (!cond.toBool()) {
                    self.ip = @intCast(@as(i32, self.ip) + offset);
                }
            },
            .halt => return,
            // ...
        }
    }
}
```

### 5. Disassembler (`src/bytecode/disasm.zig`)

Human-readable bytecode output for debugging:

```zig
pub fn disassemble(allocator: Allocator, mod: *const Module) ![]u8 {
    var disasm = Disassembler.init(mod, buffer.writer());
    try disasm.disassembleModule();
    return buffer.toOwnedSlice();
}
```

**Example Output**:
```
; Ziggy DBL Bytecode Disassembly
; Version: 0.1
; Entry Point: 0x0000

; === Constants (3) ===
;   [0000] id(tt)
;   [0001] id(Hello, World!)
;   [0002] id(customers)

; === Code (24 bytes) ===
  0000:  20 00 00        load_global @0        ; tt
  0003:  08 01 00        push_const #1         ; Hello, World!
  0006:  86 01           ch_display argc=1
  0008:  FF              halt
```

## Expression Compilation

Expressions compile to stack-based code:

### Integer Literals

```
Source: 42
Bytecode: push_i8 42

Source: 50000
Bytecode: push_i16 50000

Source: 3000000000
Bytecode: push_i32 3000000000
```

### Binary Operations

```
Source: a + b * c
Bytecode:
  load_global @a
  load_global @b
  load_global @c
  mul
  add
```

### Comparisons

```
Source: x > 10
Bytecode:
  load_global @x
  push_i8 10
  cmp_gt
```

### Function Calls

```
Source: %abs(value)
Bytecode:
  load_global @value
  fn_abs

Source: myFunc(a, b)
Bytecode:
  load_global @a
  load_global @b
  call_external #myFunc
```

## Statement Compilation

### Assignment

```
Source: x = y + 1
Bytecode:
  load_global @y
  push_i8 1
  add
  store_global @x
```

### If Statement

```
Source: if (x > 0) display(tt, "positive")
Bytecode:
  load_global @x
  push_i8 0
  cmp_gt
  jump_if_false +12    ; Skip to end
  load_global @tt
  push_const #"positive"
  ch_display 1
  ; (end)
```

### Loops

```
Source: while (x < 10) incr x
Bytecode:
  ; (loop start)
  load_global @x
  push_i8 10
  cmp_lt
  jump_if_false +8     ; Exit loop
  load_global @x
  incr
  store_global @x
  jump -14             ; Back to start
  ; (end)
```

## Label Resolution

Forward jumps are patched after code generation:

```zig
// Emit jump with placeholder
try self.emit(.jump);
const jump_loc = self.code.items.len;
try self.emitU16(0);  // Placeholder

// ... emit more code ...

// Patch the jump
fn patchJump(self: *Self, jump_loc: usize) void {
    const current = self.code.items.len;
    const offset: i16 = @intCast(current - (jump_loc + 2));
    const bytes = std.mem.toBytes(offset);
    self.code.items[jump_loc] = bytes[0];
    self.code.items[jump_loc + 1] = bytes[1];
}
```

## Constant Pool

String and numeric constants are deduplicated:

```zig
fn addStringConstant(self: *Self, str: []const u8) !u16 {
    if (self.string_constants.get(str)) |idx| {
        return idx;  // Already exists
    }
    const idx = self.constants.items.len;
    try self.constants.append(.{ .identifier = str });
    try self.string_constants.put(str, idx);
    return idx;
}
```

## Built-in Functions

Mapped directly to opcodes:

```zig
fn getBuiltinOpcode(name: []const u8) ?Opcode {
    const builtins = std.StaticStringMap(Opcode).initComptime(.{
        .{ "%abs", .fn_abs },
        .{ "%trim", .str_trim },
        .{ "%date", .fn_date },
        .{ "%size", .fn_size },
        // ...
    });
    return builtins.get(name);
}
```

## Error Handling

The compiler uses Zig's error union pattern:

```zig
pub const CompileError = error{
    OutOfMemory,
    TooManyConstants,     // > 65535 constants
    TooManyLocals,        // > 65535 locals
    TooManyGlobals,       // > 65535 globals
    UndefinedVariable,
    UndefinedLabel,
    DuplicateLabel,
    InvalidExpression,
    JumpTooFar,           // Offset > i16 range
};
```

## Future Enhancements

1. **Optimization Passes**: Constant folding, dead code elimination
2. **Debug Info**: Source maps for debugging
3. **Library Linking**: Import/export resolution
4. **JIT Compilation**: Hot path optimization
5. **Parallel Compilation**: Multi-file builds
