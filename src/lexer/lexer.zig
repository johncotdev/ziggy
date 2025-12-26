//! Ziggy DBL Lexer
//!
//! Tokenizes DBL source code into a stream of tokens.
//! DBL is case-insensitive for keywords and identifiers.

const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

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

        // Single character tokens
        const single_char_token: ?TokenType = switch (c) {
            '(' => .lparen,
            ')' => .rparen,
            '[' => .lbracket,
            ']' => .rbracket,
            ',' => .comma,
            '+' => .plus,
            '-' => .minus,
            '*' => .star,
            '/' => .slash,
            '#' => .hash,
            ':' => .colon,
            '=' => .equals,
            else => null,
        };

        if (single_char_token) |tt| {
            return self.makeToken(tt, self.source[start_pos..self.position]);
        }

        // String literals
        if (c == '"' or c == '\'') {
            return self.scanString(c, start_pos, start_col);
        }

        // Numbers
        if (std.ascii.isDigit(c)) {
            return self.scanNumber(start_pos, start_col);
        }

        // Identifiers and keywords
        if (std.ascii.isAlphabetic(c) or c == '_' or c == '$') {
            return self.scanIdentifier(start_pos, start_col);
        }

        // Dot operators (.EQ., .AND., etc.) or just a period
        if (c == '.') {
            return self.scanDotOperator(start_pos, start_col);
        }

        // Unknown character
        return self.makeToken(.invalid, self.source[start_pos..self.position]);
    }

    fn scanString(self: *Self, quote: u8, start_pos: usize, start_col: usize) Token {
        while (!self.isAtEnd() and self.peek() != quote) {
            if (self.peek() == '\n') {
                self.line += 1;
                self.column = 0;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return .{
                .type = .invalid,
                .lexeme = self.source[start_pos..self.position],
                .line = self.line,
                .column = start_col,
            };
        }

        // Consume closing quote
        _ = self.advance();

        return .{
            .type = .string_literal,
            .lexeme = self.source[start_pos..self.position],
            .line = self.line,
            .column = start_col,
        };
    }

    fn scanNumber(self: *Self, start_pos: usize, start_col: usize) Token {
        // Consume digits
        while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        // Check for decimal point
        if (!self.isAtEnd() and self.peek() == '.' and
            self.position + 1 < self.source.len and
            std.ascii.isDigit(self.source[self.position + 1]))
        {
            _ = self.advance(); // consume '.'
            while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
            return .{
                .type = .decimal_literal,
                .lexeme = self.source[start_pos..self.position],
                .line = self.line,
                .column = start_col,
            };
        }

        return .{
            .type = .integer_literal,
            .lexeme = self.source[start_pos..self.position],
            .line = self.line,
            .column = start_col,
        };
    }

    fn scanIdentifier(self: *Self, start_pos: usize, start_col: usize) Token {
        while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.peek()) or
            self.peek() == '_' or self.peek() == '$'))
        {
            _ = self.advance();
        }

        const lexeme = self.source[start_pos..self.position];
        var lower_buf: [32]u8 = undefined;
        const lower_len = @min(lexeme.len, 32);
        const lower_lexeme = std.ascii.lowerString(lower_buf[0..lower_len], lexeme[0..lower_len]);
        const token_type = keywords.get(lower_lexeme) orelse .identifier;

        return .{
            .type = token_type,
            .lexeme = lexeme,
            .line = self.line,
            .column = start_col,
        };
    }

    fn scanDotOperator(self: *Self, start_pos: usize, start_col: usize) Token {
        // Check for .EQ., .NE., .LT., .LE., .GT., .GE., .AND., .OR., .NOT., .XOR.
        if (!self.isAtEnd() and std.ascii.isAlphabetic(self.peek())) {
            const op_start = self.position;
            while (!self.isAtEnd() and std.ascii.isAlphabetic(self.peek())) {
                _ = self.advance();
            }
            if (!self.isAtEnd() and self.peek() == '.') {
                _ = self.advance();
                const op = self.source[op_start .. self.position - 1];
                var lower_buf: [8]u8 = undefined;
                const lower_op = std.ascii.lowerString(lower_buf[0..@min(op.len, 8)], op);

                const op_type: ?TokenType = if (std.mem.eql(u8, lower_op, "eq"))
                    .op_eq
                else if (std.mem.eql(u8, lower_op, "ne"))
                    .op_ne
                else if (std.mem.eql(u8, lower_op, "lt"))
                    .op_lt
                else if (std.mem.eql(u8, lower_op, "le"))
                    .op_le
                else if (std.mem.eql(u8, lower_op, "gt"))
                    .op_gt
                else if (std.mem.eql(u8, lower_op, "ge"))
                    .op_ge
                else if (std.mem.eql(u8, lower_op, "and"))
                    .op_and
                else if (std.mem.eql(u8, lower_op, "or"))
                    .op_or
                else if (std.mem.eql(u8, lower_op, "not"))
                    .op_not
                else if (std.mem.eql(u8, lower_op, "xor"))
                    .op_xor
                else
                    null;

                if (op_type) |tt| {
                    return .{
                        .type = tt,
                        .lexeme = self.source[start_pos..self.position],
                        .line = self.line,
                        .column = start_col,
                    };
                }
            }
            // Not a valid dot operator, reset
            self.position = start_pos + 1;
            self.column = start_col + 1;
        }

        // Just a period
        return .{
            .type = .period,
            .lexeme = ".",
            .line = self.line,
            .column = start_col,
        };
    }

    fn skipWhitespaceAndComments(self: *Self) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\t', '\r' => {
                    _ = self.advance();
                },
                '\n' => {
                    _ = self.advance();
                    self.line += 1;
                    self.column = 1;
                },
                ';' => {
                    // Comment - skip to end of line
                    while (!self.isAtEnd() and self.peek() != '\n') {
                        _ = self.advance();
                    }
                },
                else => break,
            }
        }
    }

    fn isAtEnd(self: *const Self) bool {
        return self.position >= self.source.len;
    }

    fn peek(self: *const Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.position];
    }

    fn advance(self: *Self) u8 {
        const c = self.source[self.position];
        self.position += 1;
        self.column += 1;
        return c;
    }

    fn makeToken(self: *const Self, token_type: TokenType, lexeme: []const u8) Token {
        return .{
            .type = token_type,
            .lexeme = lexeme,
            .line = self.line,
            .column = self.column - lexeme.len,
        };
    }

    // Keyword lookup map (case-insensitive - caller must lowercase)
    const keywords = std.StaticStringMap(TokenType).initComptime(.{
        // Program structure
        .{ "record", .kw_record },
        .{ "endrecord", .kw_endrecord },
        .{ "proc", .kw_proc },
        .{ "end", .kw_end },
        .{ "main", .kw_main },
        .{ "endmain", .kw_endmain },
        .{ "subroutine", .kw_subroutine },
        .{ "endsubroutine", .kw_endsubroutine },
        .{ "function", .kw_function },
        .{ "endfunction", .kw_endfunction },
        .{ "group", .kw_group },
        .{ "endgroup", .kw_endgroup },
        .{ "structure", .kw_structure },
        .{ "endstructure", .kw_endstructure },
        .{ "common", .kw_common },
        .{ "endcommon", .kw_endcommon },
        .{ "global", .kw_global },
        .{ "endglobal", .kw_endglobal },
        .{ "literal", .kw_literal },
        .{ "endliteral", .kw_endliteral },

        // Control flow
        .{ "if", .kw_if },
        .{ "then", .kw_then },
        .{ "else", .kw_else },
        .{ "begin", .kw_begin },
        .{ "case", .kw_case },
        .{ "endcase", .kw_endcase },
        .{ "using", .kw_using },
        .{ "endusing", .kw_endusing },
        .{ "select", .kw_select },

        // Loops
        .{ "do", .kw_do },
        .{ "until", .kw_until },
        .{ "while", .kw_while },
        .{ "for", .kw_for },
        .{ "from", .kw_from },
        .{ "thru", .kw_thru },
        .{ "foreach", .kw_foreach },
        .{ "in", .kw_in },
        .{ "forever", .kw_forever },
        .{ "repeat", .kw_repeat },
        .{ "exitloop", .kw_exitloop },
        .{ "nextloop", .kw_nextloop },

        // Subroutine/function
        .{ "xcall", .kw_xcall },
        .{ "xreturn", .kw_xreturn },
        .{ "call", .kw_call },
        .{ "return", .kw_return },
        .{ "freturn", .kw_freturn },
        .{ "mreturn", .kw_mreturn },
        .{ "goto", .kw_goto },
        .{ "stop", .kw_stop },
        .{ "exit", .kw_exit },

        // I/O
        .{ "open", .kw_open },
        .{ "close", .kw_close },
        .{ "read", .kw_read },
        .{ "reads", .kw_reads },
        .{ "write", .kw_write },
        .{ "writes", .kw_writes },
        .{ "store", .kw_store },
        .{ "delete", .kw_delete },
        .{ "find", .kw_find },
        .{ "unlock", .kw_unlock },
        .{ "flush", .kw_flush },
        .{ "display", .kw_display },
        .{ "accept", .kw_accept },

        // Data manipulation
        .{ "clear", .kw_clear },
        .{ "init", .kw_init },
        .{ "incr", .kw_incr },
        .{ "decr", .kw_decr },
        .{ "locase", .kw_locase },
        .{ "upcase", .kw_upcase },
        .{ "data", .kw_data },

        // OOP
        .{ "class", .kw_class },
        .{ "endclass", .kw_endclass },
        .{ "method", .kw_method },
        .{ "endmethod", .kw_endmethod },
        .{ "property", .kw_property },
        .{ "endproperty", .kw_endproperty },
        .{ "namespace", .kw_namespace },
        .{ "endnamespace", .kw_endnamespace },
        .{ "import", .kw_import },
        .{ "extends", .kw_extends },
        .{ "implements", .kw_implements },
        .{ "interface", .kw_interface },
        .{ "endinterface", .kw_endinterface },
        .{ "public", .kw_public },
        .{ "private", .kw_private },
        .{ "protected", .kw_protected },
        .{ "static", .kw_static },
        .{ "virtual", .kw_virtual },
        .{ "override", .kw_override },
        .{ "new", .kw_new },

        // Exception handling
        .{ "try", .kw_try },
        .{ "catch", .kw_catch },
        .{ "finally", .kw_finally },
        .{ "endtry", .kw_endtry },
        .{ "throw", .kw_throw },
        .{ "onerror", .kw_onerror },
        .{ "offerror", .kw_offerror },
    });
};

test "lexer basic tokens" {
    var lexer = Lexer.init("record\n  name ,a30\nendrecord");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.kw_record, tokens[0].type);
    try std.testing.expectEqual(TokenType.identifier, tokens[1].type);
    try std.testing.expectEqual(TokenType.comma, tokens[2].type);
}

test "lexer string literal" {
    var lexer = Lexer.init("\"Hello, World\"");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.string_literal, tokens[0].type);
    try std.testing.expectEqualStrings("\"Hello, World\"", tokens[0].lexeme);
}

test "lexer dot operators" {
    var lexer = Lexer.init(".EQ. .AND. .NOT.");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.op_eq, tokens[0].type);
    try std.testing.expectEqual(TokenType.op_and, tokens[1].type);
    try std.testing.expectEqual(TokenType.op_not, tokens[2].type);
}
