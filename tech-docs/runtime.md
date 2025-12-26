# Ziggy DBL Runtime Interpreter

## Overview

The runtime is a tree-walking interpreter that directly executes the AST. It provides an alternative to bytecode execution, useful for development and debugging.

**Location**: `src/runtime/runtime.zig`

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Runtime                                   │
│                                                                 │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐  │
│  │  Variables  │  │  Channels   │  │    Execution State     │  │
│  │             │  │             │  │                        │  │
│  │  globals    │  │  [0] term   │  │  current_line          │  │
│  │  records    │  │  [1] isam   │  │  error_state           │  │
│  │  commons    │  │  [2] seq    │  │  call_stack            │  │
│  └─────────────┘  └─────────────┘  └─────────────────────────┘  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
                           │
            ┌──────────────┴──────────────┐
            ▼                             ▼
┌───────────────────────┐     ┌───────────────────────────────────┐
│   ISAM Subsystem      │     │        Terminal I/O               │
│                       │     │                                   │
│   IsamFile            │     │   stdin/stdout                    │
│   B+ Tree             │     │   "tt:" device                    │
└───────────────────────┘     └───────────────────────────────────┘
```

## Core Components

### Runtime State

```zig
pub const Runtime = struct {
    allocator: std.mem.Allocator,

    // Variable storage
    globals: std.StringHashMap(Value),
    records: std.StringHashMap(*RecordInstance),

    // I/O channels
    channels: [MAX_CHANNELS]?*Channel,
    next_channel: u32,

    // Execution control
    current_line: u32,
    error_code: i32,
    should_stop: bool,

    // Output capture (for testing)
    output_buffer: ?*std.ArrayList(u8),
};
```

### Channel Abstraction

```zig
pub const Channel = struct {
    channel_type: ChannelType,
    is_open: bool,
    is_terminal: bool,
    filename: ?[]const u8,

    // Type-specific data
    file: ?std.fs.File,
    isam_file: ?*IsamFile,

    // Position tracking
    current_rfa: ?u48,
};

pub const ChannelType = enum {
    terminal,
    sequential,
    isam,
    relative,
};
```

## Execution Flow

### Main Entry Point

```zig
pub fn execute(self: *Self, program: *const ast.Program) !void {
    // Phase 1: Process data declarations
    for (program.statements) |stmt| {
        switch (stmt) {
            .record => |rec| try self.defineRecord(rec),
            .field => |field| try self.defineField(field),
            .common => |common| try self.defineCommon(common),
            else => {},
        }
    }

    // Phase 2: Execute procedure
    var in_proc = false;
    for (program.statements) |stmt| {
        if (self.should_stop) break;

        switch (stmt) {
            .proc => in_proc = true,
            else => {
                if (in_proc) {
                    try self.executeStatement(stmt);
                }
            },
        }
    }
}
```

### Statement Execution

```zig
fn executeStatement(self: *Self, stmt: ast.Statement) !void {
    switch (stmt) {
        .assignment => |a| try self.executeAssignment(a),
        .if_stmt => |i| try self.executeIf(i),
        .loop => |l| try self.executeLoop(l),
        .display_stmt => |d| try self.executeDisplay(d),
        .open_stmt => |o| try self.executeOpen(o),
        .read_stmt => |r| try self.executeRead(r),
        .store_stmt => |s| try self.executeStore(s),
        .xcall => |x| try self.executeXcall(x),
        .goto_stmt => |g| try self.executeGoto(g),
        // ...
    }
}
```

### Expression Evaluation

```zig
fn evaluateExpression(self: *Self, expr: ast.Expression) !Value {
    switch (expr) {
        .integer => |val| return Value{ .integer = val },
        .string => |val| return Value{ .string = val },
        .identifier => |name| return self.getVariable(name),
        .binary => |bin| return self.evaluateBinary(bin),
        .call => |call| return self.evaluateCall(call),
        // ...
    }
}
```

## Value System

```zig
pub const Value = union(enum) {
    null_val: void,
    integer: i64,
    decimal: struct { value: i64, precision: u8 },
    string: []const u8,
    alpha: struct { data: []u8, size: u16 },
    boolean: bool,
    handle: *anyopaque,
    record_ref: *RecordInstance,

    pub fn toString(self: Value, allocator: Allocator) ![]u8 {
        switch (self) {
            .integer => |i| return std.fmt.allocPrint(allocator, "{}", .{i}),
            .string => |s| return allocator.dupe(u8, s),
            .alpha => |a| return allocator.dupe(u8, a.data[0..a.size]),
            // ...
        }
    }

    pub fn toInteger(self: Value) i64 {
        switch (self) {
            .integer => |i| return i,
            .string => |s| return std.fmt.parseInt(i64, s, 10) catch 0,
            .decimal => |d| return d.value,
            // ...
        }
    }
};
```

## I/O Operations

### Opening Channels

```zig
fn executeOpen(self: *Self, open: ast.OpenStatement) !void {
    var channel_num = try self.evaluateExpression(open.channel);
    const filename = try self.evaluateExpression(open.filename);

    // Auto-assign channel if 0
    if (channel_num.integer == 0) {
        channel_num.integer = self.next_channel;
        self.next_channel += 1;
        try self.setVariable(/* target */, channel_num);
    }

    // Determine channel type
    if (std.mem.eql(u8, filename.string, "tt:")) {
        // Terminal
        self.channels[@intCast(channel_num.integer)] = &Channel{
            .channel_type = .terminal,
            .is_terminal = true,
            .is_open = true,
        };
    } else if (std.mem.indexOf(u8, open.mode, ":I") != null) {
        // ISAM
        const isam = try IsamFile.open(self.allocator, filename.string, ...);
        self.channels[@intCast(channel_num.integer)] = &Channel{
            .channel_type = .isam,
            .isam_file = isam,
            .is_open = true,
        };
    } else {
        // Sequential
        const file = try std.fs.cwd().openFile(filename.string, ...);
        self.channels[@intCast(channel_num.integer)] = &Channel{
            .channel_type = .sequential,
            .file = file,
            .is_open = true,
        };
    }
}
```

### Display Statement

```zig
fn executeDisplay(self: *Self, display: ast.DisplayStatement) !void {
    const channel_num = try self.evaluateExpression(display.channel);
    const channel = self.channels[@intCast(channel_num.integer)];

    if (channel == null or !channel.?.is_open) {
        return error.ChannelNotOpen;
    }

    // Build output string
    var output = std.ArrayList(u8).init(self.allocator);
    defer output.deinit();

    for (display.expressions) |expr| {
        const value = try self.evaluateExpression(expr);
        const str = try value.toString(self.allocator);
        defer self.allocator.free(str);
        try output.appendSlice(str);
    }
    try output.append('\n');

    // Write to channel
    if (channel.?.is_terminal) {
        const stdout = std.io.getStdOut().writer();
        try stdout.writeAll(output.items);
    } else if (channel.?.file) |f| {
        try f.writeAll(output.items);
    }
}
```

### ISAM Operations

```zig
fn executeStore(self: *Self, store: ast.StoreStatement) !void {
    const channel = try self.getChannel(store.channel);

    if (channel.channel_type != .isam) {
        return error.NotIsamChannel;
    }

    const record = try self.getRecordBuffer();
    _ = try channel.isam_file.?.store(record);
}

fn executeRead(self: *Self, read: ast.ReadStatement) !void {
    const channel = try self.getChannel(read.channel);

    if (read.key) |key_expr| {
        // Keyed read
        const key = try self.evaluateExpression(key_expr);
        const record = try channel.isam_file.?.read(
            key.toString(),
            0,  // key number
            .exact,
        );
        try self.loadRecord(read.record, record);
    } else {
        // Sequential read
        const record = try channel.isam_file.?.readNext(0);
        try self.loadRecord(read.record, record);
    }
}
```

## XCALL Dispatch

```zig
fn executeXcall(self: *Self, xcall: ast.XCallStatement) !void {
    // Normalize routine name
    var name_lower: [256]u8 = undefined;
    const name = std.ascii.lowerString(&name_lower, xcall.routine_name);

    if (std.mem.eql(u8, name, "isamc")) {
        // ISAMC - Create ISAM file
        try self.xcallIsamc(xcall.arguments);
    } else if (std.mem.eql(u8, name, "flags")) {
        // FLAGS - Set runtime flags
        try self.xcallFlags(xcall.arguments);
    } else {
        // Unknown routine
        return error.UnknownRoutine;
    }
}

fn xcallIsamc(self: *Self, args: []ast.Expression) !void {
    // xcall ISAMC(filename, record_size, num_keys, key_spec...)
    const filename = try self.evaluateExpression(args[0]);
    const record_size = try self.evaluateExpression(args[1]);
    const num_keys = try self.evaluateExpression(args[2]);

    var key_defs = std.ArrayList(KeyDef).init(self.allocator);
    defer key_defs.deinit();

    for (3..3 + num_keys.integer) |i| {
        const spec = try self.evaluateExpression(args[i]);
        const key_def = try self.parseKeySpec(spec.string);
        try key_defs.append(key_def);
    }

    try IsamFile.create(
        self.allocator,
        filename.string,
        @intCast(record_size.integer),
        key_defs.items,
    );
}
```

## Record Handling

### Record Instance

```zig
pub const RecordInstance = struct {
    definition: *const ast.RecordDef,
    buffer: []u8,  // Raw record data
    field_offsets: []u16,

    pub fn getField(self: *Self, name: []const u8) ?[]u8 {
        for (self.definition.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, name)) {
                const offset = self.field_offsets[i];
                const size = getFieldSize(field.data_type);
                return self.buffer[offset..offset + size];
            }
        }
        return null;
    }

    pub fn setField(self: *Self, name: []const u8, value: Value) !void {
        const field_data = self.getField(name) orelse return error.FieldNotFound;
        const str = try value.toString(self.allocator);
        @memcpy(field_data, str);
    }
};
```

### Field Access

```zig
fn getVariable(self: *Self, name: []const u8) !Value {
    // Check for record.field access
    if (std.mem.indexOf(u8, name, ".")) |dot| {
        const record_name = name[0..dot];
        const field_name = name[dot + 1..];

        if (self.records.get(record_name)) |record| {
            if (record.getField(field_name)) |data| {
                return Value{ .alpha = .{ .data = data, .size = @intCast(data.len) } };
            }
        }
    }

    // Check globals
    if (self.globals.get(name)) |value| {
        return value;
    }

    return error.UndefinedVariable;
}
```

## Control Flow

### If Statement

```zig
fn executeIf(self: *Self, if_s: ast.IfStatement) !void {
    const condition = try self.evaluateExpression(if_s.condition.*);

    if (condition.toBoolean()) {
        try self.executeStatement(if_s.then_branch.*);
    } else if (if_s.else_branch) |else_branch| {
        try self.executeStatement(else_branch.*);
    }
}
```

### Loop Execution

```zig
fn executeLoop(self: *Self, loop: ast.LoopStatement) !void {
    switch (loop.loop_type) {
        .do_forever => {
            while (!self.should_stop) {
                try self.executeStatement(loop.body.*);
            }
        },
        .while_loop => {
            while (!self.should_stop) {
                const cond = try self.evaluateExpression(loop.condition.?);
                if (!cond.toBoolean()) break;
                try self.executeStatement(loop.body.*);
            }
        },
        .do_until => {
            while (!self.should_stop) {
                try self.executeStatement(loop.body.*);
                const cond = try self.evaluateExpression(loop.condition.?);
                if (cond.toBoolean()) break;
            }
        },
        .for_from_thru => {
            // Initialize counter
            try self.executeExpression(loop.init_expr.?);
            while (!self.should_stop) {
                // Check end condition
                const end = try self.evaluateExpression(loop.condition.?);
                const counter = try self.getVariable(/* counter name */);
                if (counter.toInteger() > end.toInteger()) break;

                try self.executeStatement(loop.body.*);

                // Increment
                try self.executeExpression(loop.update_expr.?);
            }
        },
        // ...
    }
}
```

## Error Handling

```zig
pub const RuntimeError = error{
    OutOfMemory,
    UndefinedVariable,
    FieldNotFound,
    ChannelNotOpen,
    NotIsamChannel,
    IoError,
    UnknownRoutine,
    DivisionByZero,
    TypeMismatch,
    RecordLocked,
    EndOfFile,
};
```

The runtime captures errors and can expose them via `%error`:

```zig
fn getBuiltinFunction(self: *Self, name: []const u8) ?Value {
    if (std.mem.eql(u8, name, "%error")) {
        return Value{ .integer = self.error_code };
    }
    // ...
}
```

## Testing

The runtime supports output capture for testing:

```zig
test "display output" {
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    var runtime = Runtime.init(allocator);
    runtime.output_buffer = &output;
    defer runtime.deinit();

    try runtime.execute(&test_program);

    try std.testing.expectEqualStrings("Hello, World!\n", output.items);
}
```
