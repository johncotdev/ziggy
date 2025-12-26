//! Ziggy - Zibol Compiler and Runtime
//!
//! Usage:
//!   ziggy <file.zbl>              Run a Zibol program (interpreter)
//!   ziggy run <file.zbl|.zbc>     Run a program (auto-detect mode)
//!   ziggy compile <file.zbl>      Compile to bytecode (.zbc)
//!   ziggy disasm <file.zbl|.zbc>  Disassemble to readable output
//!   ziggy repl                    Start interactive REPL
//!   ziggy --help                  Show help
//!   ziggy --version               Show version

const std = @import("std");
const ziggy = @import("ziggy");

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
            try printErr("Usage: ziggy compile <file.zbl> [-o output.zbc]\n");
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
            try printErr("Usage: ziggy disasm <file.zbl|file.zbc>\n");
            return;
        }
        try disasmFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "run")) {
        if (args.len < 3) {
            try printErr("Error: run requires a filename\n");
            try printErr("Usage: ziggy run <file.zbl|file.zbc>\n");
            return;
        }
        try runFileAuto(allocator, args[2]);
    } else {
        // Assume it's a filename - run with interpreter
        try runFile(allocator, command);
    }
}

fn printUsage() !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.writeAll(
        \\Ziggy - Zibol Compiler and Runtime
        \\
        \\Usage:
        \\  ziggy <file.zbl>              Run a Zibol program (interpreter)
        \\  ziggy run <file.zbl|.zbc>     Run a program (auto-detect mode)
        \\  ziggy compile <file.zbl>      Compile to bytecode (.zbc)
        \\  ziggy disasm <file.zbl|.zbc>  Disassemble to readable output
        \\  ziggy repl                    Start interactive REPL
        \\  ziggy --help                  Show this help message
        \\  ziggy --version               Show version information
        \\
        \\Compile Options:
        \\  -o <file>                     Output file (default: <input>.zbc)
        \\
        \\Examples:
        \\  ziggy hello.zbl               Run hello.zbl with interpreter
        \\  ziggy compile hello.zbl       Compile to hello.zbc
        \\  ziggy compile hello.zbl -o bin/hello.zbc
        \\  ziggy run bin/hello.zbc       Run compiled bytecode
        \\  ziggy disasm hello.zbl        Show bytecode disassembly
        \\
    );
    try stdout.flush();
}

fn printVersion() !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("Ziggy version {s}\n", .{ziggy.version});
    try stdout.writeAll("Zibol - Zig Business Oriented Language\n");
    try stdout.writeAll("https://github.com/johncotdev/ziggy\n");
    try stdout.flush();
}

fn printErr(msg: []const u8) !void {
    var stderr_buffer: [1024]u8 = undefined;
    var stderr_file = std.fs.File.stderr();
    var stderr_writer = stderr_file.writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;
    try stderr.writeAll(msg);
    try stderr.flush();
}

fn printStdout(comptime fmt: []const u8, args: anytype) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    try stdout.print(fmt, args);
    try stdout.flush();
}

fn printStderr(comptime fmt: []const u8, args: anytype) !void {
    var stderr_buffer: [1024]u8 = undefined;
    var stderr_file = std.fs.File.stderr();
    var stderr_writer = stderr_file.writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;
    try stderr.print(fmt, args);
    try stderr.flush();
}

fn runFile(allocator: std.mem.Allocator, filename: []const u8) !void {
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10); // 10MB max
    defer allocator.free(source);

    ziggy.run(allocator, source) catch |err| {
        try printStderr("Runtime error: {}\n", .{err});
    };
}

fn runFileAuto(allocator: std.mem.Allocator, filename: []const u8) !void {
    // Check file extension
    if (std.mem.endsWith(u8, filename, ".zbc") or
        std.mem.endsWith(u8, filename, ".zbx"))
    {
        try runBytecodeFile(allocator, filename);
    } else {
        try runFile(allocator, filename);
    }
}

fn runBytecodeFile(allocator: std.mem.Allocator, filename: []const u8) !void {
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const bytes = try file.readToEndAlloc(allocator, 1024 * 1024 * 10); // 10MB max
    defer allocator.free(bytes);

    // Deserialize and run
    var fbs = std.io.fixedBufferStream(bytes);
    var mod = ziggy.bytecode.Module.deserialize(allocator, fbs.reader()) catch |err| {
        try printStderr("Error: Invalid bytecode file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer mod.deinit();

    var vm = ziggy.bytecode.VM.init(allocator);
    defer vm.deinit();

    vm.execute(&mod) catch |err| {
        try printStderr("VM error: {}\n", .{err});
    };
}

fn compileFile(allocator: std.mem.Allocator, filename: []const u8, output_file: ?[]const u8) !void {
    // Read source file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    defer allocator.free(source);

    // Tokenize
    var lex = ziggy.lexer.Lexer.init(source);
    const tokens = lex.tokenize(allocator) catch |err| {
        try printStderr("Lexer error: {}\n", .{err});
        return;
    };
    defer allocator.free(tokens);

    // Parse
    var parse = ziggy.parser.Parser.init(allocator, tokens);
    defer parse.deinit();
    var program = parse.parse() catch |err| {
        try printStderr("Parser error: {}\n", .{err});
        return;
    };
    defer program.deinit(allocator);

    // Compile to bytecode
    var compiler = ziggy.bytecode.Compiler.init(allocator);
    defer compiler.deinit();

    var mod = compiler.compile(&program) catch |err| {
        try printStderr("Compile error: {}\n", .{err});
        return;
    };
    defer mod.deinit();

    // Determine output filename
    const out_name = if (output_file) |of|
        of
    else blk: {
        // Replace .zbl with .zbc
        if (std.mem.endsWith(u8, filename, ".zbl") or std.mem.endsWith(u8, filename, ".ZBL")) {
            const base = filename[0 .. filename.len - 4];
            break :blk try std.fmt.allocPrint(allocator, "{s}.zbc", .{base});
        } else {
            break :blk try std.fmt.allocPrint(allocator, "{s}.zbc", .{filename});
        }
    };
    defer if (output_file == null) allocator.free(out_name);

    // Create output directory if needed
    if (std.fs.path.dirname(out_name)) |dir| {
        std.fs.cwd().makePath(dir) catch {};
    }

    // Write bytecode file
    const out_file = std.fs.cwd().createFile(out_name, .{}) catch |err| {
        try printStderr("Error: Could not create output file '{s}': {}\n", .{ out_name, err });
        return;
    };
    defer out_file.close();

    var write_buffer: [4096]u8 = undefined;
    var buffered_writer = out_file.writer(&write_buffer);

    mod.serialize(&buffered_writer.interface) catch |err| {
        try printStderr("Error: Failed to write bytecode: {}\n", .{err});
        return;
    };
    buffered_writer.interface.flush() catch |err| {
        try printStderr("Error: Failed to flush bytecode: {}\n", .{err});
        return;
    };

    try printStdout("Compiled: {s} -> {s}\n", .{ filename, out_name });
}

fn disasmFile(allocator: std.mem.Allocator, filename: []const u8) !void {
    // Read source file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    defer allocator.free(source);

    var mod: ziggy.bytecode.Module = undefined;
    var owns_module = false;

    // Check if it's a bytecode file or source file
    if (std.mem.endsWith(u8, filename, ".zbc") or
        std.mem.endsWith(u8, filename, ".zbx") or
        std.mem.endsWith(u8, filename, ".zbl"))
    {
        // Deserialize bytecode
        var fbs = std.io.fixedBufferStream(source);
        mod = ziggy.bytecode.Module.deserialize(allocator, fbs.reader()) catch |err| {
            try printStderr("Error: Invalid bytecode file: {}\n", .{err});
            return;
        };
        owns_module = true;
    } else {
        // Compile source to bytecode first
        var lex = ziggy.lexer.Lexer.init(source);
        const tokens = lex.tokenize(allocator) catch |err| {
            try printStderr("Lexer error: {}\n", .{err});
            return;
        };
        defer allocator.free(tokens);

        var parse = ziggy.parser.Parser.init(allocator, tokens);
        defer parse.deinit();
        var program = parse.parse() catch |err| {
            try printStderr("Parser error: {}\n", .{err});
            return;
        };
        defer program.deinit(allocator);

        var compiler = ziggy.bytecode.Compiler.init(allocator);
        defer compiler.deinit();

        mod = compiler.compile(&program) catch |err| {
            try printStderr("Compile error: {}\n", .{err});
            return;
        };
        owns_module = true;
    }
    defer if (owns_module) mod.deinit();

    // Disassemble
    var output: std.ArrayList(u8) = .{};
    defer output.deinit(allocator);

    var disasm = ziggy.bytecode.Disassembler.init(&mod, output.writer(allocator));
    disasm.disassembleModule() catch |err| {
        try printStderr("Disassembly error: {}\n", .{err});
        return;
    };

    // Write to stdout
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    try stdout.writeAll(output.items);
    try stdout.flush();
}

fn runRepl(allocator: std.mem.Allocator) !void {
    _ = allocator;
    try printStdout("Zibol REPL v{s}\n", .{ziggy.version});
    try printErr("REPL not yet implemented.\n");
    try printErr("Use: ziggy <file.zbl> to run a Zibol program\n");
}

test "main module loads" {
    try std.testing.expect(true);
}
