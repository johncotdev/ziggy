//! Ziggy - A Zig implementation of the Zibol language
//!
//! Zibol (Zig Business Oriented Language) is a modern business programming
//! language inspired by DIBOL/DBL and COBOL, designed for migrating and
//! modernizing legacy business applications.
//!
//! This library provides:
//! - Lexer: Tokenizes Zibol (.zbl) source code
//! - Parser: Builds an AST from tokens
//! - AST: Abstract Syntax Tree types
//! - IR: Intermediate Representation for multiple backends
//! - Runtime: Interpreter and execution environment
//! - ZiggyDB: ISAM database engine
//! - Builtins: Built-in functions and subroutines
//! - Bytecode: Bytecode compiler and VM
//! - Subroutines: XCALL subroutine registry

const std = @import("std");

// Core compiler modules
pub const lexer = @import("lexer/lexer.zig");
pub const token = @import("lexer/token.zig");
pub const parser = @import("parser/parser.zig");
pub const ast = @import("ast/ast.zig");

// Runtime modules
pub const runtime = @import("runtime/runtime.zig");
pub const builtins = @import("builtins/builtins.zig");

// Bytecode compiler and VM
pub const bytecode = @import("bytecode/bytecode.zig");

// Intermediate Representation
pub const ir = @import("ir/ir.zig");
pub const ir_lower = @import("ir/lower.zig");
pub const ir_printer = @import("ir/printer.zig");
pub const ir_emit_zig = @import("ir/emit_zig.zig");
pub const ir_emit_bytecode = @import("ir/emit_bytecode.zig");

// Subroutine registry
pub const subroutines = @import("subroutines/subroutines.zig");

// ZiggyDB - ISAM database engine
pub const isam = @import("isam/isam.zig");

/// Library version
pub const version = "0.1.0";

/// Compile and execute Zibol source code
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

/// Compile Zibol source to bytecode using the IR pipeline
pub fn compileToModule(allocator: std.mem.Allocator, source: []const u8, module_name: []const u8) !bytecode.Module {
    // Tokenize
    var lex = lexer.Lexer.init(source);
    const tokens = try lex.tokenize(allocator);
    defer allocator.free(tokens);

    // Parse
    var parse = parser.Parser.init(allocator, tokens);
    defer parse.deinit();
    var program = try parse.parse();
    defer program.deinit(allocator);

    // Lower to IR
    var lowerer = try ir_lower.Lowerer.init(allocator, module_name);
    defer lowerer.deinit();
    var ir_module = try lowerer.lowerProgram(&program);
    defer ir_module.deinit();

    // Emit bytecode from IR
    var emitter = ir_emit_bytecode.BytecodeEmitter.init(allocator);
    defer emitter.deinit();
    return try emitter.emit(&ir_module);
}

/// Lower Zibol source to IR (for debugging/inspection)
pub fn lowerToIR(allocator: std.mem.Allocator, source: []const u8, module_name: []const u8) !ir.Module {
    // Tokenize
    var lex = lexer.Lexer.init(source);
    const tokens = try lex.tokenize(allocator);
    defer allocator.free(tokens);

    // Parse
    var parse = parser.Parser.init(allocator, tokens);
    defer parse.deinit();
    var program = try parse.parse();
    defer program.deinit(allocator);

    // Lower to IR
    var lowerer = try ir_lower.Lowerer.init(allocator, module_name);
    defer lowerer.deinit();
    return try lowerer.lowerProgram(&program);
}

test "library version" {
    try std.testing.expectEqualStrings("0.1.0", version);
}
