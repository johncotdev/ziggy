**Navigation:** [1 · CLI & Pipeline](README.md) · [2 · Front-End](02-front-end.md) · [3 · Runtime & Builtins](03-runtime-and-builtins.md) · [4 · Bytecode & VM](04-bytecode-and-vm.md) · [5 · ISAM & I/O](05-isam-and-io.md)

# Tutorial 04 · Bytecode Compiler and Virtual Machine

The interpreter in Chapter 3 is great for rapid bring-up, but Ziggy also ships a full bytecode toolchain: AST → bytecode module → virtual machine execution. This chapter covers the compiler, module format, opcode set, VM loop, and disassembler so you can follow the `ziggy compile`, `ziggy run`, and `ziggy disasm` commands end to end.

---

## 1. Compiler Structure

`src/bytecode/compiler.zig` walks the AST in several passes. The top-level `compile` function collects record definitions, registers globals, emits code for statements inside `PROC`, and finally resolves labels before packaging the module.

```47:177:src/bytecode/compiler.zig
pub const Compiler = struct {
    allocator: Allocator,
    code: std.ArrayList(u8),
    constants: std.ArrayList(module.Constant),
    types: std.ArrayList(module.TypeDef),
    routines: std.ArrayList(module.RoutineDef),
    globals: std.StringHashMap(VarInfo),
    locals: std.StringHashMap(VarInfo),
    labels: std.StringHashMap(Label),
    ...

    pub fn compile(self: *Self, program: *const ast.Program) CompileError!module.Module {
        // First pass: record definitions
        for (program.statements) |stmt| {
            switch (stmt) {
                .record => |rec| try self.compileRecordDef(&rec),
                else => {},
            }
        }
        // Second pass: globals
        for (program.statements) |stmt| {
            switch (stmt) {
                .field => |field| try self.addGlobal(field.name, dataTypeFromAst(field.data_type)),
                .common => |common| { ... },
                else => {},
            }
        }
        // Third pass: emit code inside PROC
        var in_proc = false;
        for (program.statements) |stmt| {
            switch (stmt) {
                .proc => in_proc = true,
                .record, .field, ... => {},
                else => if (in_proc) try self.compileStatement(&stmt),
            }
        }

        try self.emit(.halt);
        try self.resolveLabels();
        return self.buildModule();
    }
};
```

Each statement compiles down to opcode emissions. For example, assignments push the RHS expression, resolve the target slot, and emit `store_global` or `store_local`. Since the compiler tracks `current_stack`, it can also report `StackOverflow` errors before runtime.

---

## 2. Module Format

Compiled code lives inside a `module.Module`, which resembles a traditional constant-pool bytecode format (think JVM or CLR-lite). The header stores magic bytes, version numbers, flags, and the entry-point offset. Sections follow for constants, types, routines, code, and exports/imports.

```1:123:src/bytecode/module.zig
pub const ModuleHeader = extern struct {
    magic: [4]u8,
    version_major: u16,
    version_minor: u16,
    flags: ModuleFlags,
    section_count: u32,
    entry_point: u32,
    source_hash: u32,
    _reserved: [8]u8,
    ...
};

pub const Constant = union(ConstantTag) {
    integer: i64,
    decimal: struct { value: i64, precision: u8 },
    string: []const u8,
    alpha: struct { data: []const u8, size: u16 },
    identifier: []const u8,
    record_ref: u16,
    routine_ref: u16,
    ...
};
```

`module.Module.deserialize` reads this structure back from disk. The CLI’s `disasm` subcommand simply loads the module (or compiles a source file on the fly) and hands it to the disassembler (see Section 5).

---

## 3. Opcode Set

`src/bytecode/opcodes.zig` defines a dense enum of instructions grouped by purpose—stack ops, locals/globals, arithmetic, control flow, subroutine calls, channel I/O, ISAM access, string manipulation, etc. Each opcode knows its operand width via helper methods like `operandSize()`/`name()`, which keeps the VM and disassembler in sync.

```8:178:src/bytecode/opcodes.zig
pub const Opcode = enum(u8) {
    // Stack Operations
    nop = 0x00,
    push_null = 0x01,
    push_true = 0x02,
    push_false = 0x03,
    push_i8 = 0x04,
    push_i16 = 0x05,
    push_i32 = 0x06,
    push_i64 = 0x07,
    push_const = 0x08,
    pop = 0x0A,
    ...
    // Control Flow
    jump = 0x60,
    jump_if_true = 0x62,
    jump_if_false = 0x63,
    jump_eq = 0x65,
    ...
    // Subroutine Calls
    call = 0x70,
    call_external = 0x71,
    call_native = 0x72,
    ret = 0x74,
    xcall = 0x76,
    ...
    // ISAM Operations
    isam_create = 0x90,
    isam_read = 0x93,
    isam_write = 0x95,
    ...
};
```

Having explicit opcodes for channels and ISAM keeps the VM close to DBL’s mental model—`DISPLAY` can become `ch_display`, `READ` can become `ch_read` or `isam_read`, and so on.

---

## 4. Virtual Machine Loop

`src/bytecode/vm.zig` contains a stack-based VM. It maintains instruction pointer (`ip`), stack pointer (`sp`), frame pointer (`fp`), a fixed-size value stack, globals array, heap arena, call stack, modules list, and channel/subroutine managers. The `execute` method sets the entry point and calls `run`, which fetches and dispatches opcodes in a giant `switch`.

```227:399:src/bytecode/vm.zig
pub fn execute(self: *Self, module: *const Module) VMError!void {
    self.current_module = module;
    if (module.header.entry_point == 0xFFFFFFFF) {
        return VMError.InvalidRoutine;
    }
    self.ip = module.header.entry_point;
    try self.run();
}

fn run(self: *Self) VMError!void {
    const module = self.current_module orelse return VMError.InvalidRoutine;
    while (self.ip < module.code.len) {
        const opcode: Opcode = @enumFromInt(module.code[self.ip]);
        self.ip += 1;
        switch (opcode) {
            .nop => {},
            .push_null => try self.push(.{ .null_val = {} }),
            .push_i16 => {
                const val = self.readI16(module);
                try self.push(.{ .integer = val });
            },
            ...
            .load_global => { ... },
            .store_global => { ... },
            ...
            .add => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(.{ .integer = a.toInt() + b.toInt() });
            },
            ...
        }
    }
}
```

The VM reuses many interpreter concepts: `Value` union, channel manager, and subroutine registry. That makes it feasible to share native XCALLs between interpreter and VM, and eventually run compiled modules inside the CLI’s `run` command.

---

## 5. Disassembler and Tooling

The disassembler (`src/bytecode/disasm.zig`) renders modules into a readable listing: constants, types, routines, and annotated bytecode. It powers `ziggy disasm foo.zbc` and is also a helpful debugging aid while extending the compiler or VM.

```11:131:src/bytecode/disasm.zig
pub const Disassembler = struct {
    mod: *const module.Module,
    code: []const u8,
    offset: usize,
    writer: std.ArrayList(u8).Writer,
    ...

    pub fn disassembleModule(self: *Self) !void {
        try self.writer.print("; Ziggy DBL Bytecode Disassembly\n", .{});
        ...
        try self.writer.print("; === Code ({} bytes) ===\n", .{self.code.len});
        try self.disassembleCode(0, self.code.len);
    }

    pub fn disassembleInstruction(self: *Self) !void {
        const addr = self.offset;
        const op_byte = self.code[self.offset];
        self.offset += 1;
        const op = std.meta.intToEnum(Opcode, op_byte) catch { ... };
        try self.writer.print("  {X:0>4}:  {X:0>2}", .{ addr, op_byte });
        ...
        try self.printOperandInfo(op, operand_bytes[0..operand_size]);
    }
};
```

Because both the VM and disassembler depend on the same opcode metadata, adding a new instruction is straightforward: update `opcodes.zig`, teach the compiler to emit it, extend the VM `switch`, and optionally add disassembly logic.

---

## 6. Workflow Recap

1. **`ziggy compile foo.dbl`** → Lexer/Parser build AST → `bytecode.Compiler` emits module → module serialized via `module.serialize`.
2. **`ziggy run foo.zbc`** → Module deserialized → `bytecode.VM` loads it → VM `run` executes opcodes → runtime subsystems (channels, ISAM, XCALL) handle side effects.
3. **`ziggy disasm foo.zbc`** → Module loaded → `Disassembler` prints constants/types/routines/instructions for inspection.

Armed with these pieces you can diagnose compiler bugs, add new opcodes, or instrument the VM. Chapter 5 zooms in on ISAM storage and the supporting I/O infrastructure, which both the interpreter and VM rely on for real-world DBL workloads.
