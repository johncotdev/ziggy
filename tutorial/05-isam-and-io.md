**Navigation:** [1 · CLI & Pipeline](README.md) · [2 · Front-End](02-front-end.md) · [3 · Runtime & Builtins](03-runtime-and-builtins.md) · [4 · Bytecode & VM](04-bytecode-and-vm.md) · [5 · ISAM & I/O](05-isam-and-io.md)

# Tutorial 05 · ISAM, Storage, and I/O Integration

Synergy DBL applications live and die by their data stores and channel semantics. Ziggy recreates that environment with a full ISAM implementation, reusable channel manager, and native subroutines that wire DBL statements/`XCALL`s to Zig’s filesystem APIs. This chapter finishes the tour by explaining those systems and pointing you to the supporting documentation & examples.

---

## 1. ISAM File Definitions

`src/isam/isam.zig` implements the Indexed Sequential Access Method. Key definitions describe how to extract bytes from a record into one or more key segments, whether duplicates are allowed, and how updates are handled. The `IsamFile` struct owns both the index (`.ism`) and data (`.is1`) files plus a collection of `btree.BTree` instances—one per key.

```32:176:src/isam/isam.zig
pub const KeyDef = struct {
    segments: []const KeySegment,
    allow_duplicates: bool,
    changes_allowed: bool,
    key_number: u8,

    pub const KeySegment = struct {
        start: u32,
        length: u32,
        key_type: KeyType,
    };

    pub fn keyLength(self: KeyDef) u32 { ... }
    pub fn extractKey(self: KeyDef, record: []const u8, allocator: std.mem.Allocator) ![]u8 { ... }
};

pub const IsamFile = struct {
    allocator: std.mem.Allocator,
    index_file: ?std.fs.File,
    data_file: ?std.fs.File,
    index_header: IndexHeader,
    data_header: DataHeader,
    key_defs: []KeyDef,
    btrees: []btree.BTree,
    current_key: u8,
    current_rfa: ?RFA,
    ...

    pub fn create(... ) IsamError!*Self { ... }
    pub fn open(... ) IsamError!*Self { ... }
    pub fn store(self: *Self, record: []const u8) IsamError!void { ... }
    pub fn read(self: *Self, key_num: u8, mode: MatchMode, key: []const u8, out: []u8) IsamError!void { ... }
};
```

Zig’s error unions (`IsamError!T`) keep file I/O failures, duplicate key violations, and locking issues explicit. Because the struct stores headers, filenames, and B-tree state, higher layers can switch keys (`current_key`) or iterate sequentially without reopening files.

---

## 2. B-Tree Index Implementation

`src/isam/btree.zig` provides a B+ tree tuned for ISAM workloads. Nodes keep sorted keys plus either child pointers (internal nodes) or RFA (record file address) pointers (leaf nodes), and leaves are linked together for sequential scans—a direct analogue to Synergy’s indexed files.

```7:189:src/isam/btree.zig
pub const Node = struct {
    is_leaf: bool,
    key_count: u16,
    keys: [MAX_KEYS]Key,
    children: [ORDER]?*Node,
    records: [MAX_KEYS]RecordPtr,
    next_leaf: ?*Node,
    parent: ?*Node,
    ...
    pub fn findKeyPosition(self: *const Self, key: Key) usize { ... }
    pub fn insertKeyAt(self: *Self, pos: usize, key: Key, record: RecordPtr) void { ... }
};

pub const BTree = struct {
    allocator: std.mem.Allocator,
    root: ?*Node,
    height: usize,
    size: usize,
    ...
    pub fn search(self: *Self, key: Key) ?RecordPtr { ... }
    pub fn searchWithMode(self: *Self, key: Key, mode: SearchMode) ?SearchResult { ... }
};
```

Because nodes hold raw slices (`[]const u8`) for key bytes, Zig’s allocator control matters: insertion allocates key copies, and `deinit` walks the tree to free them. That mirrors the low-level memory management DBL developers expect when they reason about ISAM buffers.

---

## 3. Native Subroutines for ISAM

The interpreter and VM don’t manipulate ISAM files directly; they call out to `XCALL`s (e.g., `ISAMC`, `ISUTL`). Ziggy implements these routines natively today. For example, `native_isamc` parses key specifications, builds `KeyDef` arrays, and calls `IsamFile.create`.

```207:270:src/subroutines/subroutines.zig
fn native_isamc(ctx: *SubroutineContext) SubroutineError!?Value {
    const isam = @import("../isam/isam.zig");
    if (ctx.args.len < 4) return SubroutineError.InvalidArgument;

    const file_spec = ctx.getArgString(0) catch return SubroutineError.InvalidArgument;
    defer ctx.allocator.free(file_spec);
    const rec_size: u32 = @intCast(ctx.getArgInt(1) catch return SubroutineError.InvalidArgument);
    const num_keys: usize = @intCast(ctx.getArgInt(2) catch return SubroutineError.InvalidArgument);
    ...

    var key_defs = std.ArrayListAligned(isam.KeyDef, null).empty;
    defer key_defs.deinit(ctx.allocator);
    ...
    const isam_file = isam.IsamFile.create(
        ctx.allocator,
        file_spec,
        key_defs.items,
        rec_size,
        .{},
    ) catch return SubroutineError.FileError;
    isam_file.close();
    return null;
}
```

These subroutines reuse helper parsers (`parseKeySpec`) and rely on the same allocator discipline as the runtime. When the bytecode linker grows support for `.zbc` subroutine libraries, you can swap in DBL implementations transparently.

---

## 4. Channel & Runtime Integration

DBL I/O statements (`OPEN`, `READ`, `WRITE`, `DISPLAY`, etc.) ultimately route through the shared `ChannelManager` (see Chapter 3) so that both sequential files and ISAM files expose a uniform channel number API. `runFileAuto` in `src/main.zig` even detects `.zbc`/`.zbx` extensions to run compiled modules through the VM instead of the interpreter.

Because channels carry `isam_file` pointers alongside `std.fs.File`, the runtime can decide at execution time whether a `READ` becomes a sequential buffered read or an indexed lookup. This mirrors Synergy DE’s behaviour and keeps the learning curve familiar.

---

## 5. Documentation and Examples

The repository includes human-readable guides alongside the code:

- `docs/` — High-level overviews (`quick-start.md`, `language-reference.md`, `bytecode-design.md`, etc.) for newcomers.
- `sde-docs/` — A structured walk-through of Synergy DBL language features (data types, statements, OOP, ISAM deep dive). Great for cross-referencing terminology.
- `tech-docs/` — Architecture notes per subsystem (runtime, bytecode, subroutines, etc.) that complement this tutorial with deeper design rationale.
- `examples/` — Sample `.dbl` programs (`hello.dbl`, `isam_demo.dbl`, `composite_key_demo.dbl`) you can compile/run to see the pipeline in action.

Use these resources together: read the docs for theory, inspect the corresponding Zig source via this tutorial, and run the example programs through `ziggy run/compile/disasm` to reinforce each concept.

---

## 6. Next Steps

You now have a map of the entire Ziggy codebase:

1. CLI entry point and tooling (Chapter 1).
2. Lexer/parser/AST front-end (Chapter 2).
3. Interpreter runtime, builtins, subroutine registry (Chapter 3).
4. Bytecode compiler, module format, VM/disassembler (Chapter 4).
5. ISAM storage, channel plumbing, docs/resources (Chapter 5).

From here you can:

- Implement missing statements by following the lexer → parser → runtime/bytecode pipeline.
- Extend the ISAM layer (e.g., add relative/printer/memory channels) and expose them through new opcodes or `XCALL`s.
- Write additional tutorial chapters focused on concrete exercises (porting a Synergy program, building tests with `std.testing`, etc.).

Enjoy exploring Zig by building a compiler and runtime you can fully understand and extend.
