**Navigation:** [1 · CLI & Pipeline](README.md) · [2 · Front-End](02-front-end.md) · [3 · Runtime & Builtins](03-runtime-and-builtins.md) · [4 · Bytecode & VM](04-bytecode-and-vm.md) · [5 · ISAM & I/O](05-isam-and-io.md)

# Tutorial 02 · Front-End: Tokens, AST, Parser

The front-end translates DBL source text into a strongly typed abstract syntax tree (AST). If you already know DBL syntax, this chapter shows how Ziggy models those constructs with Zig unions, enums, and explicit memory management.

---

## 1. Token Vocabulary

The lexer emits `Token` structs tagged with a `TokenType` enum that enumerates every DBL keyword, operator, and literal form. Each token points directly into the original source slice, so no strings are copied unless necessary.

```5:75:src/lexer/token.zig
pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,
    column: usize,
    ...
};

pub const TokenType = enum {
    // Literals
    identifier,
    string_literal,
    integer_literal,
    decimal_literal,
    ...
    // Program structure keywords
    kw_record,
    kw_endrecord,
    kw_proc,
    kw_end,
    ...
    // Exception handling keywords
    kw_try,
    kw_catch,
    kw_finally,
    kw_endtry,
    ...
};
```

Because DBL is case-insensitive, `lexer/lexer.zig` lowercases lexemes before the keyword lookup. The `StaticStringMap` defined at the bottom of the lexer file behaves like a perfect hash, so keyword recognition is O(1) without branching over dozens of strings.

---

## 2. Lexer Mechanics

`lexer.Lexer` is a simple struct with cursor fields (`position`, `line`, `column`). `tokenize` walks the entire buffer, appending tokens to an `std.ArrayListAligned`, and guarantees an explicit EOF token at the end.

```10:52:src/lexer/lexer.zig
pub const Lexer = struct {
    source: []const u8,
    position: usize,
    line: usize,
    column: usize,
    ...

    pub fn tokenize(self: *Self, allocator: std.mem.Allocator) ![]Token {
        var tokens: std.ArrayListAligned(Token, null) = .empty;
        errdefer tokens.deinit(allocator);

        while (self.position < self.source.len) {
            const token = try self.nextToken();
            try tokens.append(allocator, token);

            if (token.type == .eof) break;
        }
        ...
        return tokens.toOwnedSlice(allocator);
    }
};
```

Interesting DBL-specific cases live in helpers such as `scanDotOperator` (recognizes `.EQ.` style operators) and `scanIdentifier`, which lowercases only a scratch buffer (up to 32 bytes) to avoid allocating a brand-new string per token. Numbers are recognized as either integers or decimals depending on whether a `.` is followed by digits.

Tests such as `test "lexer dot operators"` demonstrate the intended behaviour and serve as great templates when you add new punctuation or keywords.

---

## 3. AST Data Model

Once tokenized, the parser constructs an AST defined in `src/ast/ast.zig`. Both statements and expressions are `union(enum)` values, so the compiler enforces exhaustive pattern matching whenever you interpret nodes later (runtime, compiler, etc.).

```28:103:src/ast/ast.zig
pub const Statement = union(enum) {
    // Data division
    record: RecordDef,
    group: GroupDef,
    field: FieldDef,
    ...
    // Procedure division
    proc: ProcDef,
    assignment: Assignment,
    if_stmt: IfStatement,
    ...
    // I/O
    open_stmt: OpenStatement,
    close_stmt: CloseStatement,
    ...
    // Block
    block: Block,

    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .block => |*b| b.deinit(allocator),
            .if_stmt => |*i| i.deinit(allocator),
            .expression => |*e| e.deinit(allocator),
            ...
        }
    }
};
```

This design mirrors DBL’s divisions: records/groups map to global data statements, while `proc`, `if_stmt`, etc. cover the procedure division. Each struct (e.g., `Assignment`, `DisplayStatement`, `XCallStatement`) stores just enough to interpret or compile the statement later.

Expressions use another `union(enum)` with pointers for recursive nodes (`binary`, `unary`, `call`, `member`, etc.). Each variant exposes a `deinit` method so the owning allocator can clean up any heap allocations in a deterministic order.

---

## 4. Parser Strategy

`parser.Parser` wraps the token slice and advances through it while building `ast.Statement` values. At the top level, `parseStatement` switches on the incoming token type and dispatches to the appropriate helper.

```73:142:src/parser/parser.zig
fn parseStatement(self: *Self) ParseError!ast.Statement {
    const token = self.peek();

    return switch (token.type) {
        .kw_record => self.parseRecord(),
        .kw_group => self.parseGroup(),
        ...
        .kw_open => self.parseOpen(),
        .kw_close => self.parseClose(),
        ...
        .kw_class => self.parseClass(),
        .kw_namespace => self.parseNamespace(),
        ...
        .identifier => self.parseIdentifierStatement(),
        .eof => ParseError.UnexpectedEof,
        else => self.parseExpressionStatement(),
    };
}
```

Each helper matches the DBL grammar. For example, `parseRecord` optionally consumes the record name, loops over fields until `endrecord`, and builds a `Statement{ .record = ... }`. Procedural constructs allocate nested statements on the parser’s allocator (`self.allocator.create(...)`), which keeps ownership clear: the `Program` takes responsibility for freeing those nodes when execution or compilation finishes.

Error handling mirrors DBL’s “keep going” philosophy. When a helper returns an error, the parser records it, calls `synchronize()` to skip tokens until a safe boundary (e.g., start of next statement), and resumes parsing so you get multiple diagnostics in one pass.

---

## 5. Tests and Extension Points

- **Lexer tests** (bottom of `lexer/lexer.zig`) cover strings, dot operators, and a simple `record` snippet.
- **Parser growth** is incremental: add a keyword to `TokenType`, teach the lexer to emit it, and add a branch in `parseStatement`. The exhaustive `switch` will highlight any missing handlers at compile time.
- **AST ownership** is centralized in `Program.deinit`, making it safe to attach custom data (e.g., semantic annotations) as long as you free them alongside the existing structures.

From here you can step into Chapter 3 to see how the runtime walks these AST nodes, or jump directly to Chapter 4 if you want to follow the bytecode compiler’s second pass.
