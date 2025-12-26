**Navigation:** [1 · CLI & Pipeline](README.md) · [2 · Front-End](02-front-end.md) · [3 · Runtime & Builtins](03-runtime-and-builtins.md) · [4 · Bytecode & VM](04-bytecode-and-vm.md) · [5 · ISAM & I/O](05-isam-and-io.md)

# Tutorial 03 · Runtime, Builtins, and Subroutines

With the AST in hand, Ziggy can execute DBL programs either directly (interpreter) or via bytecode (Chapter 4). This chapter focuses on the interpreter path implemented in `src/runtime/runtime.zig`, plus the supporting libraries that surface DBL’s built-in functions and XCALL ecosystem.

---

## 1. Runtime State and Values

The `Value` union represents every runtime type the interpreter understands: alpha buffers, decimals, implied decimals, integers, strings, booleans, nulls, and handles. Helper methods (`toString`, `toInteger`, `isTruthy`) capture DBL’s coercion rules in one place.

```20:90:src/runtime/runtime.zig
pub const Value = union(enum) {
    alpha: []u8,
    decimal: i64,
    implied_decimal: struct { value: i64, precision: u8 },
    integer: i64,
    string: []const u8,
    boolean: bool,
    null_val: void,
    handle: usize,
    ...

    pub fn toString(self: Value, allocator: std.mem.Allocator) ![]u8 { ... }
    pub fn toInteger(self: Value) i64 { ... }
    pub fn isTruthy(self: Value) bool { ... }
};
```

`Runtime` itself tracks global variables, record definitions, channels, a call stack, and a registry of XCALL subroutines. Everything hangs off an explicit allocator so the interpreter can clean up deterministically.

```110:200:src/runtime/runtime.zig
pub const Runtime = struct {
    allocator: std.mem.Allocator,
    variables: std.StringHashMap(Value),
    records: std.StringHashMap(ast.RecordDef),
    channels: [1024]Channel,
    call_stack: std.ArrayListAligned(CallFrame, null),
    output_buffer: std.ArrayListAligned(u8, null),
    subroutine_registry: subroutines.SubroutineRegistry,
    ...

    pub fn init(allocator: std.mem.Allocator) Self {
        var channels: [1024]Channel = undefined;
        for (&channels) |*ch| {
            ch.* = .{ .file = null, .isam_file = null, .mode = .closed, ... };
        }
        return .{
            .allocator = allocator,
            .variables = std.StringHashMap(Value).init(allocator),
            ...
            .subroutine_registry = subroutines.SubroutineRegistry.init(allocator),
        };
    }
};
```

---

## 2. Walking Statements

Execution is a giant `switch` over `ast.Statement`. Each branch delegates to a helper that knows how to evaluate expressions, mutate variables, or interact with channels. Missing statements fall into a `TODO` block so you can gradually fill out DBL coverage.

```220:247:src/runtime/runtime.zig
pub fn executeStatement(self: *Self, stmt: ast.Statement) RuntimeError!void {
    switch (stmt) {
        .record => |r| try self.executeRecord(r),
        .assignment => |a| try self.executeAssignment(a),
        .if_stmt => |i| try self.executeIf(i),
        .display_stmt => |d| try self.executeDisplay(d),
        .clear_stmt => |c| try self.executeClear(c),
        .incr_stmt => |inc| try self.executeIncr(inc),
        .block => |b| {
            for (b.statements) |s| {
                try self.executeStatement(s);
            }
        },
        .xcall => |x| try self.executeXCall(x),
        .open_stmt => |o| try self.executeOpen(o),
        .close_stmt => |c| try self.executeClose(c),
        .store_stmt => |s| try self.executeStore(s),
        .read_stmt => |r| try self.executeRead(r),
        .write_stmt => |w| try self.executeWrite(w),
        .delete_stmt => |d| try self.executeDelete(d),
        .proc => {},
        .expression => |e| {
            _ = try self.evaluateExpression(e);
        },
        else => { /* TODO */ },
    }
}
```

Since statements contain nested expressions, the interpreter ultimately relies on `evaluateExpression`, which mirrors the AST union and handles arithmetic, comparisons, and identifiers by delegating to helper methods on `Value`.

---

## 3. Built-in Functions

Synergy DBL exposes `%` functions; Ziggy implements them in `src/builtins/builtins.zig`. A compile-time `StaticStringMap` registers each builtin name to a Zig function with the signature `fn ([]const Value, Allocator) BuiltinError!Value`.

```18:80:src/builtins/builtins.zig
pub const builtins = std.StaticStringMap(BuiltinFn).initComptime(.{
    .{ "trim", trim },
    .{ "atrim", atrim },
    .{ "ltrim", ltrim },
    .{ "len", len },
    .{ "size", size },
    .{ "instr", instr },
    ...
});

pub fn trim(args: []const Value, allocator: std.mem.Allocator) BuiltinError!Value {
    if (args.len < 1) return BuiltinError.InvalidArgument;
    const str = args[0].toString(allocator) catch return BuiltinError.OutOfMemory;
    defer allocator.free(str);
    const trimmed = std.mem.trimRight(u8, str, " ");
    const result = allocator.dupe(u8, trimmed) catch return BuiltinError.OutOfMemory;
    return Value{ .string = result };
}
```

The runtime can look up a builtin by name (`builtins.getBuiltin`) and execute it using the same allocator as the caller. Numeric, math, and date/time functions showcase how easy it is to wrap Zig’s standard library operations inside DBL semantics.

---

## 4. Subroutine Registry and Stdlib

When DBL code issues `XCALL`, the runtime lowers it to `subroutines.SubroutineRegistry`. The registry supports both native Zig implementations and bytecode-backed routines (future expansion).

```76:201:src/subroutines/subroutines.zig
pub const SubroutineRegistry = struct {
    allocator: std.mem.Allocator,
    subroutines: std.StringHashMap(SubroutineDef),
    loaded_modules: std.ArrayListAligned(*bytecode.Module, null),
    ...

    fn registerNativeSubroutines(self: *Self) !void {
        try self.registerNative("isamc", native_isamc);
        try self.registerNative("isutl", native_isutl);
        try self.registerNative("flags", native_flags);
        ...
    }

    pub fn call(self: *Self, name: []const u8, ctx: *SubroutineContext) SubroutineError!?Value {
        const def = self.lookup(name) orelse return SubroutineError.SubroutineNotFound;
        switch (def.sub_type) {
            .native => return def.native_fn.? (ctx),
            .bytecode => return SubroutineError.NotImplemented,
        }
    }
};
```

The `Stdlib` manager (`src/subroutines/stdlib.zig`) builds on top of the registry: it adds additional native subroutines (`date`, `time`, `locase`, etc.), loads `.zbc` files from a configured stdlib directory, and uses the linker to expose their exports. When bytecode-backed XCALLs land, this layer will be the bridge between compiled modules and the interpreter.

---

## 5. Channel Manager and I/O Plumbing

Much of DBL revolves around channel-based I/O. The reusable `ChannelManager` lives in `src/subroutines/channels.zig` and is used both by the runtime (for `DISPLAY`, `READ`, etc.) and by native subroutines that need DBL’s channel semantics.

```9:214:src/subroutines/channels.zig
pub const ChannelManager = struct {
    allocator: std.mem.Allocator,
    channels: [MAX_CHANNELS]Channel,
    next_auto_channel: u32,
    ...

    pub fn open(self: *Self, channel_num: u32, filename: []const u8, mode: OpenMode, ch_type: ChannelType) !void {
        if (channel_num >= MAX_CHANNELS) return error.InvalidChannel;
        var ch = &self.channels[channel_num];
        if (ch.is_open) {
            ch.close(self.allocator);
        }
        ch.* = Channel.init();
        ch.channel_type = ch_type;
        ch.mode = mode;
        ch.filename = try self.allocator.dupe(u8, filename);
        switch (ch_type) {
            .terminal => ch.is_open = true,
            .sequential => { ... },
            .isam => { ... },
            else => return error.NotImplemented,
        }
    }
};
```

Interpreter code such as `executeDisplay` simply evaluates the channel expression, obtains a numeric channel, and delegates to the manager to write bytes to the appropriate destination (terminal, file, or ISAM handle).

---

## 6. Putting It Together

1. **`ziggy.run`** (Chapter 1) builds a `Runtime` and feeds it the AST.
2. **Statements** (Chapter 2) are pattern-matched in `executeStatement`, ensuring data and procedure divisions both have interpreter coverage.
3. **Builtins & XCALLs** route through the registry, which in turn can call into native Zig code or, later, bytecode modules linked at runtime.
4. **Channels** keep I/O consistent between interpreter and (future) VM by sharing the same manager.

When you implement new DBL statements, you generally touch three places:
- Lexer/parser to recognize the syntax (Chapter 2).
- AST definitions if a new struct is needed.
- Runtime helper that evaluates the statement and, if needed, new builtin/subroutine support.

Next up: Chapter 4 explores the bytecode compiler and VM, which reuse many of these runtime concepts but translate them into an instruction stream.
