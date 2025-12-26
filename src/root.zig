//! Ziggy DBL - A Zig implementation of the Synergy DBL language
//!
//! This library provides:
//! - Lexer: Tokenizes DBL source code
//! - Parser: Builds an AST from tokens
//! - AST: Abstract Syntax Tree types
//! - Runtime: Interpreter and execution environment
//! - ISAM: Indexed Sequential Access Method database
//! - Builtins: Built-in functions and subroutines
//! - Bytecode: Bytecode compiler and VM
//! - Subroutines: XCALL subroutine registry

const std = @import("std");

// Core compiler modules
pub const lexer = @import("lexer/lexer.zig");
pub const parser = @import("parser/parser.zig");
pub const ast = @import("ast/ast.zig");

// Runtime modules
pub const runtime = @import("runtime/runtime.zig");
pub const builtins = @import("builtins/builtins.zig");

// Bytecode compiler and VM
pub const bytecode = @import("bytecode/bytecode.zig");

// Subroutine registry
pub const subroutines = @import("subroutines/subroutines.zig");

// ISAM database
pub const isam = @import("isam/isam.zig");

/// Library version
pub const version = "0.1.0";

/// Compile and execute DBL source code
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

/// Compile DBL source to bytecode (future)
pub fn compile(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    _ = allocator;
    _ = source;
    @panic("Bytecode compilation not yet implemented");
}

test "library version" {
    try std.testing.expectEqualStrings("0.1.0", version);
}
