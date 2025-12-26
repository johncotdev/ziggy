# Ziggy DBL Subroutines Module

## Overview

The subroutines module provides a unified registry for XCALL dispatch, supporting both native (Zig) and DBL (bytecode) subroutines.

**Location**: `src/subroutines/`

## Architecture

```
┌──────────────────────────────────────────────────────────────────────┐
│                         Runtime                                       │
│                    executeXCall()                                     │
└───────────────────────────┬──────────────────────────────────────────┘
                            │
┌───────────────────────────▼──────────────────────────────────────────┐
│                   SubroutineRegistry                                  │
│                                                                       │
│   ┌─────────────┐    ┌─────────────┐    ┌─────────────────────────┐  │
│   │   Native    │    │  Bytecode   │    │        Stdlib           │  │
│   │ Subroutines │    │ Subroutines │    │                         │  │
│   │             │    │             │    │  ┌─────────────────────┐│  │
│   │ isamc()     │    │ from .zbc   │    │  │ Native (date, time) ││  │
│   │ flags()     │    │ modules     │    │  │ DBL (utilities)     ││  │
│   │ getlog()    │    │             │    │  └─────────────────────┘│  │
│   └─────────────┘    └─────────────┘    └─────────────────────────┘  │
└──────────────────────────────────────────────────────────────────────┘
                            │
         ┌──────────────────┼──────────────────┐
         │                  │                  │
         ▼                  ▼                  ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│  ChannelManager │ │     Linker      │ │     Stdlib      │
│                 │ │                 │ │                 │
│  I/O channels   │ │ .zbc loader     │ │ standard lib    │
│  terminal, file │ │ module search   │ │ date/time/math  │
│  isam, printer  │ │ symbol tables   │ │ string/system   │
└─────────────────┘ └─────────────────┘ └─────────────────┘
```

## Core Components

### SubroutineRegistry (`subroutines.zig`)

Central registry for all callable subroutines.

```zig
pub const SubroutineRegistry = struct {
    allocator: std.mem.Allocator,
    subroutines: std.StringHashMap(SubroutineDef),
    loaded_modules: std.ArrayListAligned(*bytecode.Module, null),

    pub fn init(allocator: std.mem.Allocator) Self;
    pub fn deinit(self: *Self) void;
    pub fn registerNative(self: *Self, name: []const u8, func: NativeSubroutineFn) !void;
    pub fn loadModule(self: *Self, path: []const u8) !void;
    pub fn lookup(self: *Self, name: []const u8) ?SubroutineDef;
    pub fn call(self: *Self, name: []const u8, ctx: *SubroutineContext) !?Value;
};
```

### SubroutineContext

Execution context passed to native subroutines:

```zig
pub const SubroutineContext = struct {
    allocator: std.mem.Allocator,
    args: []const Value,
    channels: *ChannelManager,

    pub fn getArg(self: *const SubroutineContext, index: usize) ?Value;
    pub fn getArgString(self: *const SubroutineContext, index: usize) ![]u8;
    pub fn getArgInt(self: *const SubroutineContext, index: usize) !i64;
};
```

### Native Subroutine Signature

```zig
pub const NativeSubroutineFn = *const fn (*SubroutineContext) SubroutineError!?Value;
```

## Module Files

| File | Purpose |
|------|---------|
| `subroutines.zig` | Registry, context, native implementations |
| `channels.zig` | I/O channel management |
| `linker.zig` | Bytecode module loader |
| `stdlib.zig` | Standard library manager |

## Built-in Native Subroutines

### ISAM Subroutines
- `isamc` - Create ISAM file
- `isutl` - ISAM utilities (stub)

### System Subroutines
- `flags` - Set runtime flags (stub)
- `getlog` - Get logical/environment variable
- `setlog` - Set logical/environment variable (stub)

### I/O Subroutines
- `print` - Print to printer (stub)
- `lpque` - Queue to printer (stub)

### String Subroutines
- `fill` - Fill string with character
- `copy` - Copy string
- `s_bld` - Build string (stub)
- `s_parse` - Parse string (stub)

## Stdlib Subroutines

### Date/Time
- `date` - Current date (YYYYMMDD)
- `time` - Current time (HHMMSS)
- `datetime` - Current datetime (YYYYMMDDHHMMSS)

### String Functions
- `locase` - Convert to lowercase
- `upcase` - Convert to uppercase
- `instr` - Find substring position
- `char` - Character from code

### Math Functions
- `abs` - Absolute value
- `int` - Integer part
- `frac` - Fractional part

### System Functions
- `sleep` - Pause execution (milliseconds)
- `spawn` - Execute system command

### Memory Functions
- `mem_alloc` - Allocate memory
- `mem_free` - Free memory

## Channel Manager (`channels.zig`)

Manages I/O channels for file operations:

```zig
pub const ChannelManager = struct {
    allocator: std.mem.Allocator,
    channels: [MAX_CHANNELS]Channel,
    next_auto_channel: u32,

    pub fn init(allocator: std.mem.Allocator) Self;
    pub fn open(self: *Self, channel_num: u32, filename: []const u8, mode: OpenMode, ch_type: ChannelType) !void;
    pub fn close(self: *Self, channel_num: u32) void;
    pub fn get(self: *Self, channel_num: u32) ?*Channel;
    pub fn write(self: *Self, channel_num: u32, data: []const u8) !void;
    pub fn readLine(self: *Self, channel_num: u32, buffer: []u8) !usize;
};

pub const ChannelType = enum {
    terminal,    // tt: device
    sequential,  // Sequential file
    isam,        // ISAM indexed file
    relative,    // Relative file
    printer,     // Printer output
    memory,      // Memory-mapped
};
```

## Linker (`linker.zig`)

Loads compiled bytecode modules:

```zig
pub const Linker = struct {
    allocator: std.mem.Allocator,
    loaded_modules: std.StringHashMap(*LinkedModule),
    search_paths: SearchPaths,

    pub fn init(allocator: std.mem.Allocator) Self;
    pub fn setStdlibPath(self: *Self, path: []const u8) !void;
    pub fn loadModule(self: *Self, name: []const u8) !*LinkedModule;
    pub fn loadModuleFromPath(self: *Self, path: []const u8) !*LinkedModule;
    pub fn getExports(self: *Self, module_name: []const u8) ![]const []const u8;
};
```

Module search order:
1. Exact path provided
2. Path with `.zbc` extension
3. Each search path + name + `.zbc`

Default search paths:
- Current directory (`.`)
- `$ZIGGY_LIB` environment variable
- `~/.ziggy/lib`

## Adding New Native Subroutines

1. Add function in `subroutines.zig` or `stdlib.zig`:

```zig
fn native_myfunction(ctx: *SubroutineContext) SubroutineError!?Value {
    // Get arguments
    const arg1 = ctx.getArgString(0) catch return SubroutineError.InvalidArgument;
    defer ctx.allocator.free(arg1);

    // Process and return
    const result = try ctx.allocator.dupe(u8, "result");
    return Value{ .alpha = result };
}
```

2. Register in `registerNativeSubroutines()` or `Stdlib.registerStdlibNatives()`:

```zig
try self.registerNative("myfunction", native_myfunction);
```

## Runtime Integration

The runtime calls subroutines via the registry:

```zig
fn executeXCall(self: *Self, xcall: ast.XCallStatement) RuntimeError!void {
    // Normalize routine name
    const routine_name = std.ascii.lowerString(routine_buf, xcall.routine_name);

    // Try registry first
    if (self.subroutine_registry.lookup(routine_name)) |sub_def| {
        // Build context and call
        var ctx = subroutines.SubroutineContext{ ... };
        _ = self.subroutine_registry.call(routine_name, &ctx);
        return;
    }

    // Fallback to inline implementations
}
```

## Error Handling

```zig
pub const SubroutineError = error{
    InvalidArgument,
    OutOfMemory,
    FileError,
    NotImplemented,
    SubroutineNotFound,
};
```

Subroutines return `SubroutineError!?Value`:
- Return `Value` on success with result
- Return `null` for void subroutines
- Return error for failures

## Future: DBL Subroutines

When the bytecode VM is complete, DBL subroutines will be supported:

```dbl
; stdlib/utils.dbl
subroutine pad_left
    a_string, a*
    a_length, d
    a_char, a1
proc
    while (%len(a_string) .lt. a_length)
        a_string = a_char + a_string
    return
end
```

Compiled to `.zbc` and loaded via the linker at runtime.
