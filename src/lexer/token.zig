//! Token types for Zibol Lexer

const std = @import("std");

/// A token from the source code
pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,
    column: usize,

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}('{s}' @ {}:{})", .{
            @tagName(self.type),
            self.lexeme,
            self.line,
            self.column,
        });
    }
};

/// All token types in Zibol
pub const TokenType = enum {
    // Literals
    identifier,
    string_literal,
    integer_literal,
    decimal_literal,

    // Single-character tokens
    lparen, // (
    rparen, // )
    lbracket, // [
    rbracket, // ]
    comma, // ,
    period, // .
    colon, // :
    semicolon, // ;
    plus, // +
    minus, // -
    star, // *
    slash, // /
    hash, // #
    equals, // =
    at, // @
    ampersand, // &

    // Comparison operators
    op_eq, // .EQ. or ==
    op_ne, // .NE. or !=
    op_lt, // .LT. or <
    op_le, // .LE. or <=
    op_gt, // .GT. or >
    op_ge, // .GE. or >=

    // Logical operators
    op_and, // .AND. or &&
    op_or, // .OR. or ||
    op_not, // .NOT. or !
    op_xor, // .XOR.

    // Bitwise operators
    op_band, // .BAND.
    op_bor, // .BOR.
    op_bnot, // .BNOT.
    op_bxor, // .BXOR.

    // Program structure keywords
    kw_record,
    kw_endrecord,
    kw_proc,
    kw_end,
    kw_main,
    kw_endmain,
    kw_subroutine,
    kw_endsubroutine,
    kw_function,
    kw_endfunction,
    kw_group,
    kw_endgroup,
    kw_structure,
    kw_endstructure,
    kw_common,
    kw_endcommon,
    kw_global,
    kw_endglobal,
    kw_literal,
    kw_endliteral,

    // Control flow keywords
    kw_if,
    kw_then,
    kw_else,
    kw_begin,
    kw_case,
    kw_endcase,
    kw_using,
    kw_endusing,
    kw_select,
    kw_default,

    // Loop keywords
    kw_do,
    kw_until,
    kw_while,
    kw_for,
    kw_from,
    kw_thru,
    kw_foreach,
    kw_in,
    kw_forever,
    kw_repeat,
    kw_exitloop,
    kw_nextloop,

    // Subroutine/function keywords
    kw_xcall,
    kw_xreturn,
    kw_call,
    kw_return,
    kw_freturn,
    kw_mreturn,
    kw_goto,
    kw_stop,
    kw_exit,

    // I/O keywords
    kw_open,
    kw_close,
    kw_read,
    kw_reads,
    kw_write,
    kw_writes,
    kw_store,
    kw_delete,
    kw_find,
    kw_unlock,
    kw_flush,
    kw_display,
    kw_accept,
    kw_get,
    kw_gets,
    kw_put,
    kw_puts,

    // Data manipulation keywords
    kw_clear,
    kw_init,
    kw_incr,
    kw_decr,
    kw_locase,
    kw_upcase,
    kw_data,
    kw_set,

    // OOP keywords
    kw_class,
    kw_endclass,
    kw_method,
    kw_endmethod,
    kw_property,
    kw_endproperty,
    kw_namespace,
    kw_endnamespace,
    kw_import,
    kw_extends,
    kw_implements,
    kw_interface,
    kw_endinterface,
    kw_delegate,
    kw_enddelegate,
    kw_enum,
    kw_endenum,
    kw_public,
    kw_private,
    kw_protected,
    kw_internal,
    kw_static,
    kw_virtual,
    kw_override,
    kw_abstract,
    kw_sealed,
    kw_new,
    kw_this,
    kw_parent,

    // Exception handling keywords
    kw_try,
    kw_catch,
    kw_finally,
    kw_endtry,
    kw_throw,
    kw_onerror,
    kw_offerror,

    // Async keywords
    kw_async,
    kw_await,

    // Special
    eof,
    invalid,
    newline,

    /// Check if this is a keyword token
    pub fn isKeyword(self: TokenType) bool {
        return switch (self) {
            .kw_record,
            .kw_endrecord,
            .kw_proc,
            .kw_end,
            .kw_main,
            .kw_endmain,
            .kw_subroutine,
            .kw_endsubroutine,
            .kw_function,
            .kw_endfunction,
            .kw_if,
            .kw_then,
            .kw_else,
            .kw_begin,
            .kw_do,
            .kw_while,
            .kw_for,
            => true,
            else => false,
        };
    }

    /// Check if this is an operator token
    pub fn isOperator(self: TokenType) bool {
        return switch (self) {
            .plus,
            .minus,
            .star,
            .slash,
            .hash,
            .equals,
            .op_eq,
            .op_ne,
            .op_lt,
            .op_le,
            .op_gt,
            .op_ge,
            .op_and,
            .op_or,
            .op_not,
            .op_xor,
            => true,
            else => false,
        };
    }

    /// Check if this is a literal token
    pub fn isLiteral(self: TokenType) bool {
        return switch (self) {
            .string_literal,
            .integer_literal,
            .decimal_literal,
            => true,
            else => false,
        };
    }
};

test "token format" {
    const token = Token{
        .type = .kw_record,
        .lexeme = "RECORD",
        .line = 1,
        .column = 1,
    };
    var buf: [100]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{}", .{token}) catch unreachable;
    try std.testing.expectEqualStrings("kw_record('RECORD' @ 1:1)", result);
}
