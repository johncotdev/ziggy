# Ziggy Architecture: Path to Native Compilation

This document outlines the architectural considerations for evolving Ziggy beyond the bytecode VM toward native compilation and/or Zig transpilation.

Ziggy compiles **Zibol** (Zig Business Oriented Language) - a modern business programming language inspired by DIBOL/DBL and COBOL, designed for migrating and modernizing legacy business applications.

## Current State

```
Source (.zbl) → Lexer → Parser → AST → Bytecode → VM → Runtime (Zig)
```

### What's Working Well

1. **ZiggyDB Module** - Standalone Zig library with clean API and C ABI
2. **Lexer/Parser** - Standard recursive descent, produces AST
3. **Runtime Functions** - Implemented in Zig, callable from VM

### Areas of Concern

1. **Bytecode Design** - Tightly coupled to interpreter semantics
2. **No IR Layer** - AST goes directly to bytecode
3. **Implicit Typing** - Zibol's alpha/decimal types need runtime support

## Proposed Architecture

### Phase 1: Introduce IR (Intermediate Representation)

Add a typed IR between AST and code generation:

```
AST → Type Checker → Typed IR → [Backend]
```

#### IR Design Principles

1. **SSA Form** (Static Single Assignment) - Each variable assigned once
2. **Explicit Types** - All values have known types
3. **Lowered Constructs** - High-level Zibol features reduced to primitives
4. **Platform Agnostic** - No target-specific details

#### Example IR

```
; Zibol source:
;   record
;     counter, d4
;   endrecord
;   counter = counter + 1
;   display(tt, counter)

; Typed IR:
%0 = alloca d4                    ; counter
%1 = load d4, %0                  ; read counter
%2 = const d4 1                   ; literal 1
%3 = add d4 %1, %2                ; counter + 1
store d4 %3, %0                   ; counter = result
%4 = load d4, %0                  ; reload for display
call void @rt_display_decimal(%tt, %4)
```

### Phase 2: Multiple Backends

With a stable IR, adding backends becomes straightforward:

#### Backend 1: Bytecode VM (Current)
- Lower IR to stack-based bytecode
- Interpret in existing VM
- Good for development/debugging

#### Backend 2: LLVM
- Translate IR to LLVM IR
- Use LLVM for optimization and codegen
- Native performance, cross-platform

#### Backend 3: Zig Transpiler
- Emit Zig source code from IR
- Compile with Zig compiler
- Full integration with Zig ecosystem

#### Backend 4: C Transpiler
- Emit C source code
- Maximum portability
- Easy debugging, familiar tools

## Implementation Roadmap

### Step 1: Define IR Data Structures

```zig
// src/ir/ir.zig

pub const Type = union(enum) {
    alpha: u32,           // ,a with length
    decimal: struct {     // ,d with precision
        length: u32,
        decimal_places: u8,
    },
    integer: u32,         // ,i with size
    void: void,
    ptr: *const Type,
    func: struct {
        params: []const Type,
        ret: Type,
    },
};

pub const Value = struct {
    id: u32,              // SSA ID (%0, %1, etc.)
    ty: Type,
};

pub const Instruction = union(enum) {
    // Memory
    alloca: struct { ty: Type, name: []const u8 },
    load: struct { ty: Type, ptr: Value },
    store: struct { val: Value, ptr: Value },

    // Arithmetic
    add: struct { ty: Type, lhs: Value, rhs: Value },
    sub: struct { ty: Type, lhs: Value, rhs: Value },
    mul: struct { ty: Type, lhs: Value, rhs: Value },
    div: struct { ty: Type, lhs: Value, rhs: Value },

    // Control flow
    br: struct { target: *Block },
    cond_br: struct { cond: Value, then_: *Block, else_: *Block },
    ret: ?Value,

    // Calls
    call: struct { func: []const u8, args: []Value, ret_ty: Type },

    // Constants
    const_int: struct { ty: Type, value: i64 },
    const_str: struct { value: []const u8 },
};

pub const Block = struct {
    name: []const u8,
    instructions: std.ArrayList(Instruction),
    terminator: ?Instruction,
};

pub const Function = struct {
    name: []const u8,
    params: []const struct { name: []const u8, ty: Type },
    ret_ty: Type,
    blocks: std.ArrayList(*Block),
    entry: *Block,
};

pub const Module = struct {
    name: []const u8,
    functions: std.ArrayList(*Function),
    globals: std.ArrayList(struct { name: []const u8, ty: Type }),
};
```

### Step 2: AST to IR Lowering

```zig
// src/ir/lower.zig

pub const Lowerer = struct {
    allocator: Allocator,
    module: *Module,
    current_func: ?*Function,
    current_block: ?*Block,
    value_counter: u32,

    pub fn lowerProgram(self: *Lowerer, ast: *AST) !*Module {
        // Process records (globals)
        for (ast.records) |record| {
            try self.lowerRecord(record);
        }

        // Process procedures
        for (ast.procedures) |proc| {
            try self.lowerProcedure(proc);
        }

        return self.module;
    }

    fn lowerExpression(self: *Lowerer, expr: *Expr) !Value {
        return switch (expr.*) {
            .binary_op => |op| {
                const lhs = try self.lowerExpression(op.left);
                const rhs = try self.lowerExpression(op.right);
                const result = self.newValue(lhs.ty);

                try self.emit(.{ .add = .{
                    .ty = lhs.ty,
                    .lhs = lhs,
                    .rhs = rhs,
                }});

                return result;
            },
            .identifier => |name| {
                // Look up variable, emit load
            },
            .literal => |lit| {
                // Emit constant
            },
            // ...
        };
    }
};
```

### Step 3: IR to Bytecode (Preserve Current Behavior)

```zig
// src/ir/emit_bytecode.zig

pub fn emitBytecode(module: *Module, allocator: Allocator) ![]u8 {
    var emitter = BytecodeEmitter.init(allocator);

    for (module.functions.items) |func| {
        try emitter.emitFunction(func);
    }

    return emitter.finalize();
}
```

### Step 4: IR to Zig (New Backend)

```zig
// src/ir/emit_zig.zig

pub fn emitZig(module: *Module, writer: anytype) !void {
    // Emit imports
    try writer.writeAll("const std = @import(\"std\");\n");
    try writer.writeAll("const rt = @import(\"runtime\");\n\n");

    // Emit globals
    for (module.globals.items) |global| {
        try emitGlobal(writer, global);
    }

    // Emit functions
    for (module.functions.items) |func| {
        try emitFunction(writer, func);
    }
}

fn emitFunction(writer: anytype, func: *Function) !void {
    try writer.print("pub fn {s}(", .{func.name});
    // Emit params...
    try writer.writeAll(") ");
    try emitType(writer, func.ret_ty);
    try writer.writeAll(" {\n");

    for (func.blocks.items) |block| {
        try emitBlock(writer, block);
    }

    try writer.writeAll("}\n\n");
}
```

### Step 5: IR to LLVM (Future)

```zig
// src/ir/emit_llvm.zig

// Uses LLVM C API via Zig's @cImport
const llvm = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
});

pub fn emitLLVM(module: *Module) !*llvm.LLVMModuleRef {
    const ctx = llvm.LLVMContextCreate();
    const mod = llvm.LLVMModuleCreateWithNameInContext("ziggy", ctx);
    const builder = llvm.LLVMCreateBuilderInContext(ctx);

    // Translate IR to LLVM IR...

    return mod;
}
```

## Runtime Library Strategy

The runtime library should be designed to work with all backends:

```zig
// src/runtime/exports.zig

// These functions are callable from:
// 1. VM bytecode interpreter
// 2. Native compiled code (via linking)
// 3. Transpiled Zig code (via import)

pub export fn rt_display_alpha(channel: i32, data: [*]const u8, len: u32) void {
    // Implementation
}

pub export fn rt_display_decimal(channel: i32, value: i64, decimal_places: u8) void {
    // Implementation
}

pub export fn rt_isam_open(filename: [*:0]const u8, mode: i32) ?*anyopaque {
    // Delegates to ISAM module
    return ziggy_open(filename, mode);
}

// etc.
```

## Type System Considerations

Zibol's type system is simpler than most, which helps:

| Zibol Type | IR Type | Zig Type | LLVM Type | C Type |
|----------|---------|----------|-----------|--------|
| `,a10` | `alpha(10)` | `[10]u8` | `[10 x i8]` | `char[10]` |
| `,d8` | `decimal(8,0)` | `i64` | `i64` | `int64_t` |
| `,d10.2` | `decimal(10,2)` | `i64` | `i64` | `int64_t` |
| `,i4` | `integer(4)` | `i32` | `i32` | `int32_t` |

**Note**: Decimals are stored as integers with implicit decimal point. The decimal places are tracked in the type for formatting.

## Migration Strategy

1. **Keep VM working** - Don't break current functionality
2. **Add IR alongside** - AST → IR → Bytecode path
3. **Validate equivalence** - Same behavior through both paths
4. **Add new backends** - One at a time, starting with Zig transpiler
5. **Deprecate direct bytecode** - Once IR is stable

## Benefits of This Approach

### For Native Compilation
- IR is close to machine semantics
- LLVM handles optimization
- Cross-platform from single IR

### For Zig Transpilation
- Generated code is readable
- Can mix with hand-written Zig
- Leverage Zig's safety features
- Easy debugging (source maps)

### For Development
- IR is inspectable (dump to text)
- Backends are independent
- Easier testing (compare IR output)

## Immediate Actions

1. **Create `src/ir/` directory** with IR data structures
2. **Add IR dump command** (`ziggy --emit-ir source.zbl`)
3. **Implement AST → IR lowering** for subset of Zibol
4. **Add bytecode emitter from IR**
5. **Verify behavior matches direct AST → bytecode**

## Files to Create

```
src/ir/
├── ir.zig           # IR data structures
├── lower.zig        # AST to IR lowering
├── printer.zig      # IR text format printer
├── emit_bytecode.zig # IR to bytecode
├── emit_zig.zig     # IR to Zig source (phase 2)
└── emit_llvm.zig    # IR to LLVM (phase 3)
```

## Timeline Considerations

| Phase | Effort | Outcome |
|-------|--------|---------|
| IR definition | 1-2 weeks | Stable intermediate format |
| AST → IR | 2-3 weeks | All Zibol constructs lowered |
| IR → Bytecode | 1 week | Equivalent to current |
| IR → Zig | 2-3 weeks | Transpiled output |
| IR → LLVM | 3-4 weeks | Native binaries |

## Conclusion

The key insight is that **adding an IR layer now makes future evolution much easier**. The ZiggyDB module is already well-designed for this - it's a standalone library that any backend can call. The challenge is the core language features (records, expressions, I/O) which currently go directly to bytecode.

By introducing an IR, we:
1. Create a stable interface for backends
2. Enable incremental migration
3. Make testing easier (compare IR output)
4. Open multiple compilation paths
