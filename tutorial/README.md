**Navigation:** [1 · CLI & Pipeline](README.md) · [2 · Front-End](02-front-end.md) · [3 · Runtime & Builtins](03-runtime-and-builtins.md) · [4 · Bytecode & VM](04-bytecode-and-vm.md) · [5 · ISAM & I/O](05-isam-and-io.md)

# Tutorial 01 · Learning Zig through Ziggy

Ziggy is an in-progress Synergy DBL implementation written in Zig. This makes it the perfect vehicle to learn Zig if you already understand DBL, C#, or other systems languages: every compiler phase is implemented in approachable Zig code, and most files mirror DBL concepts one-to-one.

This tutorial captures the on-ramp I just followed while "coming up to speed" on the project. Use it as a narrative tour of the codebase and a primer on the Zig language features you'll see along the way. Chapter 1 focuses on the CLI and top-level pipeline; subsequent chapters (linked above) dive into the front-end, runtime, bytecode generator, and storage stack.

> **Mindset:** Think of Zig as “modern C with first-class error handling, explicit memory management, and compile-time metaprogramming.” You stay in control, but the syntax is friendlier than C and the tooling (especially the standard library) does more for you.

---

## 1. Command-Line Entry Point (`src/main.zig`)

The CLI looks familiar if you have ever written argument parsers in C# or DBL, but it highlights a few Zig idioms: explicit allocators, error-return types (`!void`), and `try` for propagation. Functions such as `printUsage`, `runFile`, and `compileFile` stay in the same file so you can see end-to-end flow without juggling modules.

```15:64:src/main.zig
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage();
        return;
    }

    const command = args[1];

    if (std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "-h")) {
        try printUsage();
    } else if (std.mem.eql(u8, command, "--version") or std.mem.eql(u8, command, "-v")) {
        try printVersion();
    } else if (std.mem.eql(u8, command, "repl")) {
        try runRepl(allocator);
    } else if (std.mem.eql(u8, command, "compile")) {
        if (args.len < 3) {
            try printErr("Error: compile requires a filename\n");
            try printErr("Usage: ziggy compile <file.dbl> [-o output.zbc]\n");
            return;
        }
        const output_file = if (args.len >= 5 and std.mem.eql(u8, args[3], "-o"))
            args[4]
        else
            null;
        try compileFile(allocator, args[2], output_file);
    } else if (std.mem.eql(u8, command, "disasm")) {
        if (args.len < 3) {
            try printErr("Error: disasm requires a filename\n");
            try printErr("Usage: ziggy disasm <file.dbl|file.zbc>\n");
            return;
        }
        try disasmFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "run")) {
        if (args.len < 3) {
            try printErr("Error: run requires a filename\n");
            try printErr("Usage: ziggy run <file.dbl|file.zbc>\n");
            return;
        }
        try runFileAuto(allocator, args[2]);
    } else {
        // Assume it's a filename - run with interpreter
        try runFile(allocator, command);
    }
}
```

### Things to notice

- `defer` works like `finally` in C#: the allocator cleans itself up even if errors bubble out.
- `try` is sugar for “propagate the error upward.” No exceptions—just return values.
- Zig’s `if`/`else if` reads like C, but `const` defaults ensure values are immutable unless you explicitly use `var`.

---

## 2. The Core Pipeline (`src/root.zig`)

`ziggy.run` wires the classic compiler stages: lexer → parser → runtime interpreter. Each stage receives the project-wide allocator, so you can swap in arena or bump allocators later if performance demands.

```36:52:src/root.zig
pub fn run(allocator: std.mem.Allocator, source: []const u8) !void {
    // Tokenize
    var lex = lexer.Lexer.init(source);
    const tokens = try lex.tokenize(allocator);
    defer allocator.free(tokens);

    // Parse
    var parse = parser.Parser.init(allocator, tokens);
    defer parse.deinit();
    var program = try parse.parse();
    defer program.deinit(allocator);

    // Execute
    var rt = runtime.Runtime.init(allocator);
    defer rt.deinit();
    try rt.execute(program);
}
```

**Key Zig takeaways:** method calls look like C structs but can be invoked with `Type.method(&value, args...)`. Error sets (`!void`) keep stages honest—you can’t accidentally ignore a failed parse.

---

## 3. Lexing DBL (`src/lexer/lexer.zig`)

The lexer is a stateful struct with hand-written scanners, which should feel natural if you’ve ever built a DBL “input routine.” Zig’s slices (`[]const u8`) let you point directly into the original source without copying.

```10:88:src/lexer/lexer.zig
pub const Lexer = struct {
    source: []const u8,
    position: usize,
    line: usize,
    column: usize,

    const Self = @This();

    /// Initialize lexer with source code
    pub fn init(source: []const u8) Self {
        return .{
            .source = source,
            .position = 0,
            .line = 1,
            .column = 1,
        };
    }

    /// Tokenize entire source into token array
    pub fn tokenize(self: *Self, allocator: std.mem.Allocator) ![]Token {
        var tokens: std.ArrayListAligned(Token, null) = .empty;
        errdefer tokens.deinit(allocator);

        while (self.position < self.source.len) {
            const token = try self.nextToken();
            try tokens.append(allocator, token);

            if (token.type == .eof) break;
        }

        // Ensure EOF token
        if (tokens.items.len == 0 or tokens.items[tokens.items.len - 1].type != .eof) {
            try tokens.append(allocator, .{
                .type = .eof,
                .lexeme = "",
                .line = self.line,
                .column = self.column,
            });
        }

        return tokens.toOwnedSlice(allocator);
    }

    /// Get next token from source
    pub fn nextToken(self: *Self) !Token {
        self.skipWhitespaceAndComments();

        if (self.isAtEnd()) {
            return self.makeToken(.eof, "");
        }

        const start_pos = self.position;
        const start_col = self.column;
        const c = self.advance();
```

### Things to notice

- Methods are normal functions with the first parameter spelled out (`self: *Self`). There is no implicit `this`.
- `std.ArrayListAligned` mirrors `List<T>` but forces you to pass the allocator when you append.
- Dot keywords (`.EQ.`, `.AND.`) map cleanly into Zig enum variants, so this file doubles as a reference for DBL’s lexical grammar.

---

## 4. Parsing Statements into an AST (`src/parser/parser.zig`)

The parser demonstrates Zig’s `union(enum)`-based AST plus exhaustive `switch` statements. Each DBL keyword translates to one parse function, so when you add a statement you immediately know where to plug in the code.

```73:142:src/parser/parser.zig
/// Parse a single statement
fn parseStatement(self: *Self) ParseError!ast.Statement {
    const token = self.peek();

    return switch (token.type) {
        // Data division
        .kw_record => self.parseRecord(),
        .kw_group => self.parseGroup(),
        .kw_literal => self.parseLiteral(),
        .kw_common => self.parseCommon(),
        .kw_structure => self.parseStructure(),

        // Procedure division
        .kw_proc => self.parseProc(),
        .kw_if => self.parseIf(),
        .kw_case => self.parseCase(),
        .kw_using => self.parseUsing(),
        .kw_do => self.parseDoLoop(),
        .kw_while => self.parseWhileLoop(),
        .kw_for => self.parseForLoop(),
        .kw_foreach => self.parseForeach(),
        .kw_begin => self.parseBlock(),

        // Subroutine/function
        .kw_xcall => self.parseXCall(),
        .kw_call => self.parseCall(),
        .kw_xreturn, .kw_freturn, .kw_mreturn, .kw_return => self.parseReturn(),
        .kw_goto => self.parseGoto(),
        .kw_stop => self.parseStop(),
        .kw_exitloop => self.parseExitLoop(),
        .kw_nextloop => self.parseNextLoop(),

        // I/O
        .kw_open => self.parseOpen(),
        .kw_close => self.parseClose(),
        .kw_read => self.parseRead(),
        .kw_reads => self.parseReads(),
        .kw_write => self.parseWrite(),
        .kw_writes => self.parseWrites(),
        .kw_display => self.parseDisplay(),
        .kw_store => self.parseStore(),
        .kw_delete => self.parseDelete(),

        // Data manipulation
        .kw_clear => self.parseClear(),
        .kw_init => self.parseInit(),
        .kw_incr => self.parseIncr(),
        .kw_decr => self.parseDecr(),

        // OOP
        .kw_class => self.parseClass(),
        .kw_namespace => self.parseNamespace(),

        // End keywords (handled by parent parsers)
        .kw_end,
        .kw_endrecord,
        .kw_endgroup,
        .kw_endcase,
        .kw_endusing,
        .kw_endclass,
        .kw_endnamespace,
        => ParseError.InvalidStatement,

        // Identifier could be label, assignment, or function call
        .identifier => self.parseIdentifierStatement(),

        .eof => ParseError.UnexpectedEof,

        else => self.parseExpressionStatement(),
    };
}
```

### Things to notice

- `switch` must be exhaustive over the `TokenType` enum. The compiler will force you to handle new keywords—very helpful when growing the grammar.
- `ParseError` is an error set (`error{...}`) dedicated to the parser. Each function either returns a valid AST node or one of these explicit errors.
- `ast.Statement` is a tagged union defined in `src/ast/ast.zig`, so you get type-safe pattern matching later in the runtime.

---

## 5. Runtime Execution (`src/runtime/runtime.zig`)

The interpreter walks the AST and binds it to DBL runtime concepts: variables, records, channels, and XCALL-able subroutines. This file is also a practical guide to Zig’s tagged unions, `switch` destructuring, and manual memory management.

```220:317:src/runtime/runtime.zig
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
        .proc => {}, // PROC is just a marker
        .expression => |e| {
            _ = try self.evaluateExpression(e);
        },
        else => {
            // TODO: Implement remaining statements
        },
    }
}

fn executeDisplay(self: *Self, display: ast.DisplayStatement) RuntimeError!void {
    const channel_num = (try self.evaluateExpression(display.channel)).toInteger();

    // Build output string
    var output = std.ArrayListAligned(u8, null).empty;
    defer output.deinit(self.allocator);

    for (display.expressions) |expr| {
        const value = try self.evaluateExpression(expr);
        const str = try value.toString(self.allocator);
        defer self.allocator.free(str);
        output.appendSlice(self.allocator, str) catch {};
    }
    output.append(self.allocator, '\n') catch {};

    // Write to appropriate destination
    if (channel_num > 0 and channel_num < 1024) {
        const channel = &self.channels[@intCast(channel_num)];
        if (channel.is_terminal or channel.mode != .closed) {
            if (channel.is_terminal) {
                self.writeOutput(output.items);
            } else if (channel.file) |file| {
                _ = file.write(output.items) catch {};
            }
            return;
        }
    }

    self.writeOutput(output.items);
}
```

### Things to notice

- Tagged unions (`ast.Statement`) make it easy to destructure specific fields (`|r|`, `|x|`) without casting.
- `RuntimeError` enumerates the DBL-level failures you expect (undefined variables, key not found, etc.), so the interpreter can bubble them back to the CLI cleanly.
- Channels are stored in a fixed-size array—no hidden allocations unless you open real files.

---

## 6. Where to Go Next

1. **Chapter 2 – Front-End:** Follow `02-front-end.md` for a deeper look at tokens, the AST unions, and how the parser keeps DBL semantics intact.
2. **Chapter 3 – Runtime & Builtins:** `03-runtime-and-builtins.md` ties interpreter execution to the builtin/subroutine ecosystem and channel manager.
3. **Chapter 4 – Bytecode & VM:** When you’re ready for compilation, `04-bytecode-and-vm.md` walks through the module format, opcodes, and VM loop.
4. **Chapter 5 – ISAM & I/O:** `05-isam-and-io.md` digs into the ISAM filesystem, B-tree index, and how CLI/runtime map to real storage.

By following the full sequence you should feel comfortable reading and extending Ziggy’s codebase while simultaneously picking up core Zig idioms. When you want hands-on practice, add a DBL feature end-to-end (lexer → parser → runtime) or wire it into the bytecode pipeline using the new reference chapters.
