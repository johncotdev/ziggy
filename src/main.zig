//! Ziggy DBL - Command Line Interface
//!
//! Usage:
//!   ziggy <file.dbl>           Run a DBL program
//!   ziggy repl                 Start interactive REPL
//!   ziggy --help               Show help
//!   ziggy --version            Show version

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
    } else {
        // Assume it's a filename
        try runFile(allocator, command);
    }
}

fn printUsage() !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.writeAll(
        \\Ziggy DBL - A Zig implementation of Synergy DBL
        \\
        \\Usage:
        \\  ziggy <file.dbl>     Run a DBL program
        \\  ziggy repl           Start interactive REPL
        \\  ziggy --help         Show this help message
        \\  ziggy --version      Show version information
        \\
        \\Examples:
        \\  ziggy hello.dbl      Run hello.dbl
        \\  ziggy repl           Start REPL session
        \\
    );
    try stdout.flush();
}

fn printVersion() !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("Ziggy DBL version {s}\n", .{ziggy.version});
    try stdout.writeAll("A Zig implementation of Synergy DBL\n");
    try stdout.writeAll("https://github.com/johncotdev/ziggy\n");
    try stdout.flush();
}

fn runFile(allocator: std.mem.Allocator, filename: []const u8) !void {
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        var stderr_buffer: [1024]u8 = undefined;
        var stderr_file = std.fs.File.stderr();
        var stderr_writer = stderr_file.writer(&stderr_buffer);
        const stderr = &stderr_writer.interface;
        try stderr.print("Error: Could not open file '{s}': {}\n", .{ filename, err });
        try stderr.flush();
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10); // 10MB max
    defer allocator.free(source);

    ziggy.run(allocator, source) catch |err| {
        var stderr_buffer: [1024]u8 = undefined;
        var stderr_file = std.fs.File.stderr();
        var stderr_writer = stderr_file.writer(&stderr_buffer);
        const stderr = &stderr_writer.interface;
        try stderr.print("Runtime error: {}\n", .{err});
        try stderr.flush();
    };
}

fn runRepl(allocator: std.mem.Allocator) !void {
    _ = allocator;
    // TODO: REPL requires updated I/O for Zig 0.15
    std.debug.print("Ziggy DBL REPL v{s}\n", .{ziggy.version});
    std.debug.print("REPL not yet implemented for Zig 0.15\n", .{});
    std.debug.print("Use: ziggy <file.dbl> to run a DBL program\n", .{});
}

test "main module loads" {
    // Basic smoke test
    try std.testing.expect(true);
}
