//! Zibol Channel Manager
//!
//! Manages I/O channels for file operations, terminal I/O, and ISAM files.
//! Channels are numbered 0-255 and can be bound to various I/O targets.

const std = @import("std");
const isam = @import("../isam/isam.zig");

/// Maximum number of channels
pub const MAX_CHANNELS: usize = 256;

/// Channel types
pub const ChannelType = enum {
    terminal,    // tt: device
    sequential,  // Sequential file
    isam,        // ISAM indexed file
    relative,    // Relative file
    printer,     // Printer output
    memory,      // Memory-mapped
};

/// Channel open modes
pub const OpenMode = enum {
    input,       // I - Read only
    output,      // O - Write only (create/truncate)
    update,      // U - Read/write
    append,      // A - Append mode
};

/// Lock modes for ISAM
pub const LockMode = enum {
    none,        // No locking
    automatic,   // Lock on read, unlock on next read
    manual,      // Explicit LOCK/UNLOCK
};

/// A single I/O channel
pub const Channel = struct {
    channel_type: ChannelType,
    mode: OpenMode,
    is_open: bool,
    filename: ?[]const u8,

    // File handles
    file: ?std.fs.File,
    isam_file: ?*isam.IsamFile,

    // State tracking
    current_rfa: ?u48,           // Current record position
    current_key_num: u8,         // Current key for ISAM
    lock_mode: LockMode,
    is_locked: bool,
    eof_reached: bool,

    // Buffer for sequential reads
    line_buffer: ?[]u8,

    const Self = @This();

    pub fn init() Self {
        return Self{
            .channel_type = .terminal,
            .mode = .input,
            .is_open = false,
            .filename = null,
            .file = null,
            .isam_file = null,
            .current_rfa = null,
            .current_key_num = 0,
            .lock_mode = .none,
            .is_locked = false,
            .eof_reached = false,
            .line_buffer = null,
        };
    }

    pub fn close(self: *Self, allocator: std.mem.Allocator) void {
        if (self.file) |f| {
            f.close();
            self.file = null;
        }
        if (self.isam_file) |isf| {
            isf.close();
            allocator.destroy(isf);
            self.isam_file = null;
        }
        if (self.filename) |fname| {
            allocator.free(fname);
            self.filename = null;
        }
        if (self.line_buffer) |buf| {
            allocator.free(buf);
            self.line_buffer = null;
        }
        self.is_open = false;
        self.eof_reached = false;
        self.current_rfa = null;
    }
};

/// Channel manager - owns all channels
pub const ChannelManager = struct {
    allocator: std.mem.Allocator,
    channels: [MAX_CHANNELS]Channel,
    next_auto_channel: u32,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        var mgr = Self{
            .allocator = allocator,
            .channels = undefined,
            .next_auto_channel = 1,
        };

        // Initialize all channels
        for (&mgr.channels) |*ch| {
            ch.* = Channel.init();
        }

        // Channel 0 is typically reserved for terminal
        mgr.channels[0].channel_type = .terminal;
        mgr.channels[0].is_open = true;

        return mgr;
    }

    pub fn deinit(self: *Self) void {
        for (&self.channels) |*ch| {
            ch.close(self.allocator);
        }
    }

    /// Get next available channel number (for auto-assignment)
    pub fn getNextChannel(self: *Self) ?u32 {
        const start = self.next_auto_channel;
        var ch = start;

        while (true) {
            if (!self.channels[ch].is_open) {
                self.next_auto_channel = if (ch + 1 >= MAX_CHANNELS) 1 else ch + 1;
                return ch;
            }
            ch = if (ch + 1 >= MAX_CHANNELS) 1 else ch + 1;
            if (ch == start) return null; // All channels in use
        }
    }

    /// Open a channel
    pub fn open(self: *Self, channel_num: u32, filename: []const u8, mode: OpenMode, ch_type: ChannelType) !void {
        if (channel_num >= MAX_CHANNELS) return error.InvalidChannel;

        var ch = &self.channels[channel_num];
        if (ch.is_open) {
            ch.close(self.allocator);
        }

        ch.* = Channel.init();
        ch.channel_type = ch_type;
        ch.mode = mode;
        ch.filename = try self.allocator.dupe(u8, filename);

        switch (ch_type) {
            .terminal => {
                ch.is_open = true;
            },
            .sequential => {
                const flags: std.fs.File.OpenFlags = switch (mode) {
                    .input => .{ .mode = .read_only },
                    .output => .{ .mode = .write_only },
                    .update => .{ .mode = .read_write },
                    .append => .{ .mode = .write_only },
                };

                ch.file = if (mode == .output or mode == .append)
                    std.fs.cwd().createFile(filename, .{ .truncate = mode == .output }) catch return error.FileNotFound
                else
                    std.fs.cwd().openFile(filename, flags) catch return error.FileNotFound;

                ch.is_open = true;
            },
            .isam => {
                const isam_file = try self.allocator.create(isam.IsamFile);
                errdefer self.allocator.destroy(isam_file);

                isam_file.* = isam.IsamFile.open(self.allocator, filename, .{}) catch |err| {
                    return switch (err) {
                        error.FileNotFound => error.FileNotFound,
                        else => error.IoError,
                    };
                };

                ch.isam_file = isam_file;
                ch.is_open = true;
            },
            .relative => {
                return error.NotImplemented;
            },
            .printer => {
                return error.NotImplemented;
            },
            .memory => {
                return error.NotImplemented;
            },
        }
    }

    /// Close a channel
    pub fn close(self: *Self, channel_num: u32) void {
        if (channel_num >= MAX_CHANNELS) return;
        self.channels[channel_num].close(self.allocator);
    }

    /// Get a channel (returns null if not open)
    pub fn get(self: *Self, channel_num: u32) ?*Channel {
        if (channel_num >= MAX_CHANNELS) return null;
        const ch = &self.channels[channel_num];
        if (!ch.is_open) return null;
        return ch;
    }

    /// Check if channel is open
    pub fn isOpen(self: *Self, channel_num: u32) bool {
        if (channel_num >= MAX_CHANNELS) return false;
        return self.channels[channel_num].is_open;
    }

    /// Write to a channel
    pub fn write(self: *Self, channel_num: u32, data: []const u8) !void {
        const ch = self.get(channel_num) orelse return error.ChannelNotOpen;

        switch (ch.channel_type) {
            .terminal => {
                const stdout = std.io.getStdOut().writer();
                try stdout.writeAll(data);
            },
            .sequential => {
                if (ch.file) |f| {
                    try f.writeAll(data);
                } else {
                    return error.ChannelNotOpen;
                }
            },
            .isam => {
                return error.InvalidOperation;
            },
            else => {
                return error.NotImplemented;
            },
        }
    }

    /// Read a line from a channel
    pub fn readLine(self: *Self, channel_num: u32, buffer: []u8) !usize {
        const ch = self.get(channel_num) orelse return error.ChannelNotOpen;

        switch (ch.channel_type) {
            .terminal => {
                const stdin = std.io.getStdIn().reader();
                const line = stdin.readUntilDelimiterOrEof(buffer, '\n') catch |err| {
                    return switch (err) {
                        error.EndOfStream => error.EndOfFile,
                        else => error.IoError,
                    };
                };
                if (line) |l| {
                    return l.len;
                } else {
                    ch.eof_reached = true;
                    return error.EndOfFile;
                }
            },
            .sequential => {
                if (ch.file) |f| {
                    const reader = f.reader();
                    const line = reader.readUntilDelimiterOrEof(buffer, '\n') catch |err| {
                        return switch (err) {
                            error.EndOfStream => error.EndOfFile,
                            else => error.IoError,
                        };
                    };
                    if (line) |l| {
                        return l.len;
                    } else {
                        ch.eof_reached = true;
                        return error.EndOfFile;
                    }
                } else {
                    return error.ChannelNotOpen;
                }
            },
            else => {
                return error.InvalidOperation;
            },
        }
    }
};

/// Channel errors
pub const ChannelError = error{
    InvalidChannel,
    ChannelNotOpen,
    ChannelInUse,
    FileNotFound,
    IoError,
    EndOfFile,
    InvalidOperation,
    NotImplemented,
};

test "channel manager init" {
    const allocator = std.testing.allocator;
    var mgr = ChannelManager.init(allocator);
    defer mgr.deinit();

    // Channel 0 should be open (terminal)
    try std.testing.expect(mgr.isOpen(0));

    // Other channels should be closed
    try std.testing.expect(!mgr.isOpen(1));
}

test "channel auto-assignment" {
    const allocator = std.testing.allocator;
    var mgr = ChannelManager.init(allocator);
    defer mgr.deinit();

    const ch1 = mgr.getNextChannel();
    try std.testing.expect(ch1 != null);
    try std.testing.expectEqual(@as(u32, 1), ch1.?);
}
