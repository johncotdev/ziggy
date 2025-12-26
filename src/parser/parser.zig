//! Ziggy DBL Parser
//!
//! Parses a stream of tokens into an Abstract Syntax Tree.

const std = @import("std");
const Token = @import("../lexer/token.zig").Token;
const TokenType = @import("../lexer/token.zig").TokenType;
const ast = @import("../ast/ast.zig");

pub const ParseError = error{
    UnexpectedToken,
    UnexpectedEof,
    InvalidExpression,
    InvalidStatement,
    InvalidDataType,
    OutOfMemory,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const Token,
    current: usize,
    errors: std.ArrayListAligned(Error, null),

    const Self = @This();

    pub const Error = struct {
        message: []const u8,
        token: Token,
    };

    /// Initialize parser with tokens
    pub fn init(allocator: std.mem.Allocator, tokens: []const Token) Self {
        return .{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .errors = .empty,
        };
    }

    /// Clean up parser resources
    pub fn deinit(self: *Self) void {
        self.errors.deinit(self.allocator);
    }

    /// Parse tokens into a program
    pub fn parse(self: *Self) ParseError!ast.Program {
        var statements: std.ArrayListAligned(ast.Statement, null) = .empty;
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit(self.allocator);
        }

        while (!self.isAtEnd()) {
            if (self.parseStatement()) |stmt| {
                try statements.append(self.allocator, stmt);
            } else |err| {
                // Record error and try to recover
                self.synchronize();
                if (err == ParseError.OutOfMemory) return err;
            }
        }

        return ast.Program{
            .statements = statements.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
            .allocator = self.allocator,
        };
    }

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

    // ============================================================
    // Data Division Parsing
    // ============================================================

    fn parseRecord(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'record'

        const name: ?[]const u8 = if (self.check(.identifier))
            self.advance().lexeme
        else
            null;

        var fields: std.ArrayListAligned(ast.FieldDef, null) = .empty;
        errdefer fields.deinit(self.allocator);

        while (!self.check(.kw_endrecord) and !self.isAtEnd()) {
            const field = try self.parseFieldDef();
            try fields.append(self.allocator, field);
        }

        _ = try self.consume(.kw_endrecord, "Expected 'endrecord'");

        return ast.Statement{
            .record = .{
                .name = name,
                .fields = fields.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
            },
        };
    }

    fn parseFieldDef(self: *Self) ParseError!ast.FieldDef {
        const name = try self.consume(.identifier, "Expected field name");
        _ = try self.consume(.comma, "Expected ',' after field name");
        const data_type = try self.parseDataType();

        var initial_value: ?ast.Expression = null;
        if (self.match(&[_]TokenType{.comma})) {
            initial_value = try self.parseExpression();
        }

        return ast.FieldDef{
            .name = name.lexeme,
            .data_type = data_type,
            .initial_value = initial_value,
            .array_dims = null,
        };
    }

    fn parseDataType(self: *Self) ParseError!ast.DataType {
        const token = self.advance();

        // Check for type identifiers: a, d, i1, i2, i4, i8, p, etc.
        if (token.type == .identifier) {
            const lexeme = token.lexeme;

            // Alpha type
            if (lexeme.len >= 1 and (lexeme[0] == 'a' or lexeme[0] == 'A')) {
                if (lexeme.len == 1) {
                    return ast.DataType{ .alpha = .{ .size = null } };
                }
                if (lexeme[1] == '*') {
                    return ast.DataType{ .alpha = .{ .size = null } };
                }
                const size = std.fmt.parseInt(usize, lexeme[1..], 10) catch return ParseError.InvalidDataType;
                return ast.DataType{ .alpha = .{ .size = size } };
            }

            // Decimal type
            if (lexeme.len >= 1 and (lexeme[0] == 'd' or lexeme[0] == 'D')) {
                if (lexeme.len == 1) {
                    return ast.DataType{ .decimal = .{ .size = null } };
                }
                // Check for implied decimal (contains '.')
                if (std.mem.indexOf(u8, lexeme, ".")) |dot_pos| {
                    const total = std.fmt.parseInt(usize, lexeme[1..dot_pos], 10) catch return ParseError.InvalidDataType;
                    const prec = std.fmt.parseInt(usize, lexeme[dot_pos + 1 ..], 10) catch return ParseError.InvalidDataType;
                    return ast.DataType{ .implied_decimal = .{ .total_digits = total, .precision = prec } };
                }
                const size = std.fmt.parseInt(usize, lexeme[1..], 10) catch return ParseError.InvalidDataType;
                return ast.DataType{ .decimal = .{ .size = size } };
            }

            // Integer types
            if (lexeme.len == 2 and (lexeme[0] == 'i' or lexeme[0] == 'I')) {
                return ast.DataType{
                    .integer = switch (lexeme[1]) {
                        '1' => .i1,
                        '2' => .i2,
                        '4' => .i4,
                        '8' => .i8,
                        else => return ParseError.InvalidDataType,
                    },
                };
            }

            // String type
            if (std.ascii.eqlIgnoreCase(lexeme, "string")) {
                return ast.DataType{ .string = {} };
            }
        }

        // Structure reference @name
        if (token.type == .at) {
            const struct_name = try self.consume(.identifier, "Expected structure name after '@'");
            return ast.DataType{ .structure_ref = struct_name.lexeme };
        }

        return ParseError.InvalidDataType;
    }

    fn parseGroup(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'group'
        const name = try self.consume(.identifier, "Expected group name");

        var fields: std.ArrayListAligned(ast.FieldDef, null) = .empty;
        errdefer fields.deinit(self.allocator);

        while (!self.check(.kw_endgroup) and !self.isAtEnd()) {
            const field = try self.parseFieldDef();
            try fields.append(self.allocator, field);
        }

        _ = try self.consume(.kw_endgroup, "Expected 'endgroup'");

        return ast.Statement{
            .group = .{
                .name = name.lexeme,
                .fields = fields.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
                .overlay_target = null,
            },
        };
    }

    fn parseLiteral(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'literal'
        // TODO: Implement literal block parsing
        return ParseError.InvalidStatement;
    }

    fn parseCommon(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'common'
        // TODO: Implement common block parsing
        return ParseError.InvalidStatement;
    }

    fn parseStructure(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'structure'
        // TODO: Implement structure parsing
        return ParseError.InvalidStatement;
    }

    // ============================================================
    // Procedure Division Parsing
    // ============================================================

    fn parseProc(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'proc'
        return ast.Statement{ .proc = .{} };
    }

    fn parseIf(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'if'

        // Parse condition (may be in parentheses)
        const condition = try self.allocator.create(ast.Expression);
        errdefer self.allocator.destroy(condition);
        condition.* = try self.parseExpression();

        // Optional 'then'
        _ = self.match(&[_]TokenType{.kw_then});

        // Parse then branch
        const then_branch = try self.allocator.create(ast.Statement);
        errdefer self.allocator.destroy(then_branch);
        then_branch.* = try self.parseStatement();

        // Optional else branch
        var else_branch: ?*ast.Statement = null;
        if (self.match(&[_]TokenType{.kw_else})) {
            else_branch = try self.allocator.create(ast.Statement);
            else_branch.?.* = try self.parseStatement();
        }

        return ast.Statement{
            .if_stmt = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
            },
        };
    }

    fn parseCase(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'case'
        // TODO: Implement case statement parsing
        return ParseError.InvalidStatement;
    }

    fn parseUsing(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'using'
        // TODO: Implement using statement parsing
        return ParseError.InvalidStatement;
    }

    fn parseDoLoop(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'do'
        // TODO: Implement do loop parsing
        return ParseError.InvalidStatement;
    }

    fn parseWhileLoop(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'while'
        // TODO: Implement while loop parsing
        return ParseError.InvalidStatement;
    }

    fn parseForLoop(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'for'
        // TODO: Implement for loop parsing
        return ParseError.InvalidStatement;
    }

    fn parseForeach(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'foreach'
        // TODO: Implement foreach parsing
        return ParseError.InvalidStatement;
    }

    fn parseBlock(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'begin'

        var statements: std.ArrayListAligned(ast.Statement, null) = .empty;
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit(self.allocator);
        }

        while (!self.check(.kw_end) and !self.isAtEnd()) {
            const stmt = try self.parseStatement();
            try statements.append(self.allocator, stmt);
        }

        _ = try self.consume(.kw_end, "Expected 'end' after block");

        return ast.Statement{
            .block = .{
                .statements = statements.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
            },
        };
    }

    // ============================================================
    // Subroutine/Function Parsing
    // ============================================================

    fn parseXCall(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'xcall'
        const routine = try self.consume(.identifier, "Expected routine name");

        var arguments: std.ArrayListAligned(ast.Expression, null) = .empty;
        errdefer arguments.deinit(self.allocator);

        if (self.match(&[_]TokenType{.lparen})) {
            if (!self.check(.rparen)) {
                try arguments.append(self.allocator, try self.parseExpression());
                while (self.match(&[_]TokenType{.comma})) {
                    try arguments.append(self.allocator, try self.parseExpression());
                }
            }
            _ = try self.consume(.rparen, "Expected ')' after arguments");
        }

        return ast.Statement{
            .xcall = .{
                .routine_name = routine.lexeme,
                .arguments = arguments.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
            },
        };
    }

    fn parseCall(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'call'
        const label = try self.consume(.identifier, "Expected label name");

        return ast.Statement{
            .call = .{
                .label = label.lexeme,
            },
        };
    }

    fn parseReturn(self: *Self) ParseError!ast.Statement {
        const token = self.advance();
        const return_type: ast.ReturnType = switch (token.type) {
            .kw_xreturn => .xreturn,
            .kw_freturn => .freturn,
            .kw_mreturn => .mreturn,
            else => .simple_return,
        };

        var value: ?ast.Expression = null;
        if (return_type == .freturn or return_type == .mreturn) {
            if (!self.isAtEnd() and !self.checkEndOfStatement()) {
                value = try self.parseExpression();
            }
        }

        return ast.Statement{
            .return_stmt = .{
                .return_type = return_type,
                .value = value,
            },
        };
    }

    fn parseGoto(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'goto'
        const label = try self.consume(.identifier, "Expected label name");

        return ast.Statement{
            .goto_stmt = .{
                .label = label.lexeme,
            },
        };
    }

    fn parseStop(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'stop'
        // TODO: Parse optional exit code
        return ast.Statement{ .return_stmt = .{ .return_type = .xreturn, .value = null } };
    }

    fn parseExitLoop(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'exitloop'
        // TODO: Handle exitloop N
        return ParseError.InvalidStatement;
    }

    fn parseNextLoop(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'nextloop'
        return ParseError.InvalidStatement;
    }

    // ============================================================
    // I/O Parsing
    // ============================================================

    fn parseOpen(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'open'
        _ = try self.consume(.lparen, "Expected '(' after 'open'");

        const channel = try self.parseExpression();
        _ = try self.consume(.comma, "Expected ',' after channel");

        const mode = try self.consume(.identifier, "Expected file mode");
        _ = try self.consume(.comma, "Expected ',' after mode");

        const filename = try self.parseExpression();

        _ = try self.consume(.rparen, "Expected ')' after filename");

        return ast.Statement{
            .open_stmt = .{
                .channel = channel,
                .mode = mode.lexeme,
                .filename = filename,
                .qualifiers = &[_]ast.Qualifier{},
            },
        };
    }

    fn parseClose(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'close'
        _ = try self.consume(.lparen, "Expected '(' after 'close'");
        const channel = try self.parseExpression();
        _ = try self.consume(.rparen, "Expected ')' after channel");

        return ast.Statement{
            .close_stmt = .{
                .channel = channel,
            },
        };
    }

    fn parseRead(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'read'
        // TODO: Implement read parsing
        return ParseError.InvalidStatement;
    }

    fn parseReads(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'reads'
        // TODO: Implement reads parsing
        return ParseError.InvalidStatement;
    }

    fn parseWrite(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'write'
        // TODO: Implement write parsing
        return ParseError.InvalidStatement;
    }

    fn parseWrites(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'writes'
        // TODO: Implement writes parsing
        return ParseError.InvalidStatement;
    }

    fn parseDisplay(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'display'
        _ = try self.consume(.lparen, "Expected '(' after 'display'");

        const channel = try self.parseExpression();

        var expressions: std.ArrayListAligned(ast.Expression, null) = .empty;
        errdefer expressions.deinit(self.allocator);

        while (self.match(&[_]TokenType{.comma})) {
            try expressions.append(self.allocator, try self.parseExpression());
        }

        _ = try self.consume(.rparen, "Expected ')' after display arguments");

        return ast.Statement{
            .display_stmt = .{
                .channel = channel,
                .expressions = expressions.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
            },
        };
    }

    fn parseStore(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'store'
        // TODO: Implement store parsing
        return ParseError.InvalidStatement;
    }

    fn parseDelete(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'delete'
        // TODO: Implement delete parsing
        return ParseError.InvalidStatement;
    }

    // ============================================================
    // Data Manipulation
    // ============================================================

    fn parseClear(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'clear'
        const target = try self.parseExpression();

        return ast.Statement{
            .clear_stmt = .{
                .target = target,
            },
        };
    }

    fn parseInit(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'init'
        const target = try self.parseExpression();

        return ast.Statement{
            .init_stmt = .{
                .target = target,
            },
        };
    }

    fn parseIncr(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'incr'
        const target = try self.parseExpression();

        var amount: ?ast.Expression = null;
        if (self.match(&[_]TokenType{.comma})) {
            amount = try self.parseExpression();
        }

        return ast.Statement{
            .incr_stmt = .{
                .target = target,
                .amount = amount,
            },
        };
    }

    fn parseDecr(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'decr'
        // Similar to incr
        return self.parseIncr();
    }

    // ============================================================
    // OOP
    // ============================================================

    fn parseClass(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'class'
        // TODO: Implement class parsing
        return ParseError.InvalidStatement;
    }

    fn parseNamespace(self: *Self) ParseError!ast.Statement {
        _ = self.advance(); // consume 'namespace'
        // TODO: Implement namespace parsing
        return ParseError.InvalidStatement;
    }

    // ============================================================
    // Expression Parsing
    // ============================================================

    fn parseIdentifierStatement(self: *Self) ParseError!ast.Statement {
        // Could be: label, assignment, or expression
        const ident = self.advance();

        // Check for label (identifier followed by comma at statement level)
        if (self.check(.comma)) {
            return ast.Statement{
                .label = .{
                    .name = ident.lexeme,
                },
            };
        }

        // Check for assignment
        if (self.check(.equals)) {
            _ = self.advance(); // consume '='
            const value = try self.parseExpression();
            return ast.Statement{
                .assignment = .{
                    .target = ast.Expression{ .identifier = ident.lexeme },
                    .value = value,
                },
            };
        }

        // Otherwise, back up and parse as expression
        self.current -= 1;
        return self.parseExpressionStatement();
    }

    fn parseExpressionStatement(self: *Self) ParseError!ast.Statement {
        const expr = try self.parseExpression();
        return ast.Statement{ .expression = expr };
    }

    fn parseExpression(self: *Self) ParseError!ast.Expression {
        return self.parseOr();
    }

    fn parseOr(self: *Self) ParseError!ast.Expression {
        var expr = try self.parseAnd();

        while (self.match(&[_]TokenType{ .op_or })) {
            const right = try self.parseAnd();
            const binary = try self.allocator.create(ast.BinaryExpr);
            binary.* = .{
                .left = expr,
                .operator = .logical_or,
                .right = right,
            };
            expr = ast.Expression{ .binary = binary };
        }

        return expr;
    }

    fn parseAnd(self: *Self) ParseError!ast.Expression {
        var expr = try self.parseEquality();

        while (self.match(&[_]TokenType{ .op_and })) {
            const right = try self.parseEquality();
            const binary = try self.allocator.create(ast.BinaryExpr);
            binary.* = .{
                .left = expr,
                .operator = .logical_and,
                .right = right,
            };
            expr = ast.Expression{ .binary = binary };
        }

        return expr;
    }

    fn parseEquality(self: *Self) ParseError!ast.Expression {
        var expr = try self.parseComparison();

        while (self.match(&[_]TokenType{ .op_eq, .op_ne })) {
            const op: ast.BinaryOp = if (self.previous().type == .op_eq) .equal else .not_equal;
            const right = try self.parseComparison();
            const binary = try self.allocator.create(ast.BinaryExpr);
            binary.* = .{
                .left = expr,
                .operator = op,
                .right = right,
            };
            expr = ast.Expression{ .binary = binary };
        }

        return expr;
    }

    fn parseComparison(self: *Self) ParseError!ast.Expression {
        var expr = try self.parseTerm();

        while (self.match(&[_]TokenType{ .op_lt, .op_le, .op_gt, .op_ge })) {
            const prev = self.previous();
            const op: ast.BinaryOp = switch (prev.type) {
                .op_lt => .less_than,
                .op_le => .less_equal,
                .op_gt => .greater_than,
                .op_ge => .greater_equal,
                else => unreachable,
            };
            const right = try self.parseTerm();
            const binary = try self.allocator.create(ast.BinaryExpr);
            binary.* = .{
                .left = expr,
                .operator = op,
                .right = right,
            };
            expr = ast.Expression{ .binary = binary };
        }

        return expr;
    }

    fn parseTerm(self: *Self) ParseError!ast.Expression {
        var expr = try self.parseFactor();

        while (self.match(&[_]TokenType{ .plus, .minus })) {
            const op: ast.BinaryOp = if (self.previous().type == .plus) .add else .subtract;
            const right = try self.parseFactor();
            const binary = try self.allocator.create(ast.BinaryExpr);
            binary.* = .{
                .left = expr,
                .operator = op,
                .right = right,
            };
            expr = ast.Expression{ .binary = binary };
        }

        return expr;
    }

    fn parseFactor(self: *Self) ParseError!ast.Expression {
        var expr = try self.parseUnary();

        while (self.match(&[_]TokenType{ .star, .slash })) {
            const op: ast.BinaryOp = if (self.previous().type == .star) .multiply else .divide;
            const right = try self.parseUnary();
            const binary = try self.allocator.create(ast.BinaryExpr);
            binary.* = .{
                .left = expr,
                .operator = op,
                .right = right,
            };
            expr = ast.Expression{ .binary = binary };
        }

        return expr;
    }

    fn parseUnary(self: *Self) ParseError!ast.Expression {
        if (self.match(&[_]TokenType{ .minus, .op_not })) {
            const op: ast.UnaryOp = if (self.previous().type == .minus) .negate else .logical_not;
            const operand = try self.parseUnary();
            const unary = try self.allocator.create(ast.UnaryExpr);
            unary.* = .{
                .operator = op,
                .operand = operand,
            };
            return ast.Expression{ .unary = unary };
        }

        return self.parsePrimary();
    }

    fn parsePrimary(self: *Self) ParseError!ast.Expression {
        const token = self.peek();

        switch (token.type) {
            .integer_literal => {
                _ = self.advance();
                const value = std.fmt.parseInt(i64, token.lexeme, 10) catch 0;
                return ast.Expression{ .integer = value };
            },
            .decimal_literal => {
                _ = self.advance();
                return ast.Expression{ .decimal = token.lexeme };
            },
            .string_literal => {
                _ = self.advance();
                // Remove quotes
                const content = if (token.lexeme.len >= 2)
                    token.lexeme[1 .. token.lexeme.len - 1]
                else
                    token.lexeme;
                return ast.Expression{ .string = content };
            },
            .identifier => {
                _ = self.advance();
                return ast.Expression{ .identifier = token.lexeme };
            },
            .lparen => {
                _ = self.advance();
                const expr = try self.parseExpression();
                _ = try self.consume(.rparen, "Expected ')' after expression");
                const grouping = try self.allocator.create(ast.Expression);
                grouping.* = expr;
                return ast.Expression{ .grouping = grouping };
            },
            else => return ParseError.InvalidExpression,
        }
    }

    // ============================================================
    // Utility Functions
    // ============================================================

    fn synchronize(self: *Self) void {
        _ = self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().type == .kw_end) return;

            switch (self.peek().type) {
                .kw_record,
                .kw_proc,
                .kw_if,
                .kw_for,
                .kw_while,
                .kw_do,
                .kw_class,
                .kw_subroutine,
                .kw_function,
                => return,
                else => {},
            }

            _ = self.advance();
        }
    }

    fn consume(self: *Self, expected: TokenType, message: []const u8) ParseError!Token {
        if (self.check(expected)) {
            return self.advance();
        }

        try self.errors.append(self.allocator, .{
            .message = message,
            .token = self.peek(),
        });

        return ParseError.UnexpectedToken;
    }

    fn match(self: *Self, types: []const TokenType) bool {
        for (types) |t| {
            if (self.check(t)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *const Self, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == token_type;
    }

    fn checkEndOfStatement(self: *const Self) bool {
        const t = self.peek().type;
        return t == .eof or t == .kw_end or t == .kw_else;
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return self.previous();
    }

    fn isAtEnd(self: *const Self) bool {
        return self.peek().type == .eof;
    }

    fn peek(self: *const Self) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *const Self) Token {
        return self.tokens[self.current - 1];
    }
};

test "parser basic record" {
    const allocator = std.testing.allocator;

    const tokens = [_]Token{
        .{ .type = .kw_record, .lexeme = "record", .line = 1, .column = 1 },
        .{ .type = .identifier, .lexeme = "name", .line = 2, .column = 3 },
        .{ .type = .comma, .lexeme = ",", .line = 2, .column = 7 },
        .{ .type = .identifier, .lexeme = "a30", .line = 2, .column = 8 },
        .{ .type = .kw_endrecord, .lexeme = "endrecord", .line = 3, .column = 1 },
        .{ .type = .eof, .lexeme = "", .line = 3, .column = 10 },
    };

    var p = Parser.init(allocator, &tokens);
    defer p.deinit();
    const program = try p.parse();
    defer program.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), program.statements.len);
}
