//! Ziggy DBL Runtime
//!
//! Executes DBL programs by walking the AST.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const isam = @import("../isam/isam.zig");
const subroutines = @import("../subroutines/subroutines.zig");

pub const RuntimeError = error{
    UndefinedVariable,
    TypeMismatch,
    DivisionByZero,
    FileNotOpen,
    KeyNotFound,
    OutOfMemory,
    InvalidOperation,
};

/// Runtime value types
pub const Value = union(enum) {
    alpha: []u8,
    decimal: i64,
    implied_decimal: struct { value: i64, precision: u8 },
    integer: i64,
    string: []const u8,
    boolean: bool,
    null_val: void,
    handle: usize,

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .alpha => |a| try writer.print("{s}", .{a}),
            .decimal, .integer => |d| try writer.print("{d}", .{d}),
            .implied_decimal => |id| {
                const divisor = std.math.pow(i64, 10, id.precision);
                const whole = @divTrunc(id.value, divisor);
                const frac = @abs(@rem(id.value, divisor));
                try writer.print("{d}.{d}", .{ whole, frac });
            },
            .string => |s| try writer.print("{s}", .{s}),
            .boolean => |b| try writer.print("{}", .{b}),
            .null_val => try writer.writeAll("null"),
            .handle => |h| try writer.print("<handle:{d}>", .{h}),
        }
    }

    /// Convert value to string
    pub fn toString(self: Value, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .alpha => |a| try allocator.dupe(u8, a),
            .decimal, .integer => |d| try std.fmt.allocPrint(allocator, "{d}", .{d}),
            .string => |s| try allocator.dupe(u8, s),
            .boolean => |b| try allocator.dupe(u8, if (b) "1" else "0"),
            else => try allocator.dupe(u8, ""),
        };
    }

    /// Convert value to integer
    pub fn toInteger(self: Value) i64 {
        return switch (self) {
            .alpha => |a| std.fmt.parseInt(i64, a, 10) catch 0,
            .decimal, .integer => |d| d,
            .implied_decimal => |id| @divTrunc(id.value, std.math.pow(i64, 10, id.precision)),
            .string => |s| std.fmt.parseInt(i64, s, 10) catch 0,
            .boolean => |b| if (b) @as(i64, 1) else 0,
            else => 0,
        };
    }

    /// Check if value is truthy
    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .alpha => |a| a.len > 0 and !std.mem.allEqual(u8, a, ' '),
            .decimal, .integer => |d| d != 0,
            .implied_decimal => |id| id.value != 0,
            .string => |s| s.len > 0,
            .boolean => |b| b,
            .null_val => false,
            .handle => |h| h != 0,
        };
    }
};

/// File channel state
pub const Channel = struct {
    file: ?std.fs.File,
    isam_file: ?*isam.IsamFile,
    mode: FileMode,
    current_record: ?[]u8,
    is_locked: bool,
    is_terminal: bool,
    key_of_reference: u8, // Current key index for READS (set by READ/FIND)

    pub const FileMode = enum {
        closed,
        input,
        output,
        update,
        append,
    };
};

/// Runtime state
pub const Runtime = struct {
    allocator: std.mem.Allocator,
    variables: std.StringHashMap(Value),
    records: std.StringHashMap(ast.RecordDef), // Track record definitions
    channels: [1024]Channel,
    call_stack: std.ArrayListAligned(CallFrame, null),
    output_buffer: std.ArrayListAligned(u8, null),
    subroutine_registry: subroutines.SubroutineRegistry,

    const Self = @This();

    const CallFrame = struct {
        return_address: usize,
        locals: std.StringHashMap(Value),
    };

    /// Initialize runtime
    pub fn init(allocator: std.mem.Allocator) Self {
        var channels: [1024]Channel = undefined;
        for (&channels) |*ch| {
            ch.* = .{
                .file = null,
                .isam_file = null,
                .mode = .closed,
                .current_record = null,
                .is_locked = false,
                .is_terminal = false,
                .key_of_reference = 0, // Default to primary key
            };
        }

        return .{
            .allocator = allocator,
            .variables = std.StringHashMap(Value).init(allocator),
            .records = std.StringHashMap(ast.RecordDef).init(allocator),
            .channels = channels,
            .call_stack = .empty,
            .output_buffer = .empty,
            .subroutine_registry = subroutines.SubroutineRegistry.init(allocator),
        };
    }

    /// Put a variable, freeing any old alpha buffer
    fn putVariable(self: *Self, name: []const u8, value: Value) !void {
        // Free old alpha buffer if present
        if (self.variables.get(name)) |old| {
            if (old == .alpha) {
                self.allocator.free(old.alpha);
            }
        }
        try self.variables.put(name, value);
    }

    /// Clean up runtime
    pub fn deinit(self: *Self) void {
        // Close all open channels
        for (&self.channels) |*ch| {
            if (ch.file) |*f| {
                f.close();
            }
            if (ch.isam_file) |isam_file| {
                isam_file.close();
            }
            if (ch.current_record) |rec| {
                self.allocator.free(rec);
            }
        }

        // Free variables - only free alpha values which are heap-allocated
        // String values are slices into the source code and should not be freed
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            switch (entry.value_ptr.*) {
                .alpha => |a| self.allocator.free(a),
                else => {},
            }
        }
        self.variables.deinit();
        self.records.deinit();

        // Free call stack
        for (self.call_stack.items) |*frame| {
            frame.locals.deinit();
        }
        self.call_stack.deinit(self.allocator);
        self.output_buffer.deinit(self.allocator);

        // Free subroutine registry
        self.subroutine_registry.deinit();
    }

    /// Write to stdout
    fn writeOutput(self: *Self, data: []const u8) void {
        // Write directly to stdout
        var stdout_buf: [4096]u8 = undefined;
        var stdout_file = std.fs.File.stdout();
        var stdout_writer = stdout_file.writer(&stdout_buf);
        stdout_writer.interface.writeAll(data) catch {};
        stdout_writer.interface.flush() catch {};
        _ = self;
    }

    /// Execute a program
    pub fn execute(self: *Self, program: ast.Program) RuntimeError!void {
        for (program.statements) |stmt| {
            try self.executeStatement(stmt);
        }
    }

    /// Execute a single statement
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

    fn executeRecord(self: *Self, record: ast.RecordDef) RuntimeError!void {
        // Initialize fields to default values
        for (record.fields) |field| {
            const value = try self.createDefaultValue(field.data_type);
            self.putVariable(field.name, value) catch return RuntimeError.OutOfMemory;
        }

        // Store record definition if it has a name
        if (record.name) |name| {
            try self.records.put(name, record);
        }
    }

    fn executeAssignment(self: *Self, assignment: ast.Assignment) RuntimeError!void {
        const value = try self.evaluateExpression(assignment.value);

        switch (assignment.target) {
            .identifier => |name| {
                self.putVariable(name, value) catch return RuntimeError.OutOfMemory;
            },
            else => {
                // TODO: Handle member access, array indexing, etc.
            },
        }
    }

    fn executeIf(self: *Self, if_stmt: ast.IfStatement) RuntimeError!void {
        const condition = try self.evaluateExpression(if_stmt.condition.*);

        if (condition.isTruthy()) {
            try self.executeStatement(if_stmt.then_branch.*);
        } else if (if_stmt.else_branch) |else_branch| {
            try self.executeStatement(else_branch.*);
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
                // Terminal channel or open file - write to stdout for terminal
                if (channel.is_terminal) {
                    self.writeOutput(output.items);
                } else if (channel.file) |file| {
                    _ = file.write(output.items) catch {};
                }
                return;
            }
        }

        // Default: write to stdout (for channel 0 or unrecognized)
        self.writeOutput(output.items);
    }

    fn executeClear(self: *Self, clear: ast.ClearStatement) RuntimeError!void {
        switch (clear.target) {
            .identifier => |name| {
                if (self.variables.get(name)) |current| {
                    const cleared = switch (current) {
                        .alpha => |a| Value{ .alpha = blk: {
                            @memset(a, ' ');
                            break :blk a;
                        } },
                        .decimal, .integer => Value{ .integer = 0 },
                        .implied_decimal => |id| Value{ .implied_decimal = .{ .value = 0, .precision = id.precision } },
                        .string => Value{ .string = "" },
                        else => current,
                    };
                    try self.variables.put(name, cleared);
                }
            },
            else => {},
        }
    }

    fn executeIncr(self: *Self, incr: ast.IncrStatement) RuntimeError!void {
        const amount: i64 = if (incr.amount) |amt|
            (try self.evaluateExpression(amt)).toInteger()
        else
            1;

        switch (incr.target) {
            .identifier => |name| {
                if (self.variables.get(name)) |current| {
                    const new_val = current.toInteger() + amount;
                    self.putVariable(name, Value{ .integer = new_val }) catch return RuntimeError.OutOfMemory;
                }
            },
            else => {},
        }
    }

    fn executeXCall(self: *Self, xcall: ast.XCallStatement) RuntimeError!void {
        // Normalize routine name to lowercase
        const routine_buf = self.allocator.alloc(u8, xcall.routine_name.len) catch return RuntimeError.OutOfMemory;
        defer self.allocator.free(routine_buf);
        const routine_name = std.ascii.lowerString(routine_buf, xcall.routine_name);

        // Try the subroutine registry first
        if (self.subroutine_registry.lookup(routine_name)) |sub_def| {
            // Convert args to Values for the subroutine context
            var args = std.ArrayListAligned(Value, null).empty;
            defer {
                for (args.items) |arg| {
                    if (arg == .alpha) {
                        self.allocator.free(arg.alpha);
                    }
                }
                args.deinit(self.allocator);
            }

            for (xcall.arguments) |arg_expr| {
                const val = try self.evaluateExpression(arg_expr);
                args.append(self.allocator, val) catch return RuntimeError.OutOfMemory;
            }

            // Create dummy channel manager for now (registry needs channels)
            var channel_mgr = subroutines.ChannelManager.init(self.allocator);
            defer channel_mgr.deinit();

            var ctx = subroutines.SubroutineContext{
                .allocator = self.allocator,
                .args = args.items,
                .channels = &channel_mgr,
            };

            // Call via registry
            _ = self.subroutine_registry.call(routine_name, &ctx) catch |err| {
                switch (err) {
                    subroutines.SubroutineError.NotImplemented => {
                        // Fall through to legacy implementations
                    },
                    else => return RuntimeError.InvalidOperation,
                }
            };

            // If we get here and it was found, we're done
            if (sub_def.native_fn != null) return;
        }

        // Fallback to legacy inline implementations
        if (std.mem.eql(u8, routine_name, "isamc")) {
            try self.executeIsamc(xcall.arguments);
        }
        // Unknown routines are silently ignored for now
    }

    /// ISAMC - Create an ISAM file
    /// xcall ISAMC(file_spec, rec_size, num_keys, key_spec, ...)
    fn executeIsamc(self: *Self, args: []ast.Expression) RuntimeError!void {
        if (args.len < 4) return RuntimeError.InvalidOperation;

        // Parse file specification
        const file_spec_val = try self.evaluateExpression(args[0]);
        const file_spec = try file_spec_val.toString(self.allocator);
        defer self.allocator.free(file_spec);

        // Parse record size
        const rec_size_val = try self.evaluateExpression(args[1]);
        const rec_size: u32 = @intCast(rec_size_val.toInteger());

        // Parse number of keys
        const num_keys_val = try self.evaluateExpression(args[2]);
        const num_keys: usize = @intCast(num_keys_val.toInteger());

        if (num_keys == 0 or num_keys > 255) return RuntimeError.InvalidOperation;

        // Parse key specifications
        var key_defs = std.ArrayListAligned(isam.KeyDef, null).empty;
        defer key_defs.deinit(self.allocator);

        var key_segments_storage = std.ArrayListAligned([]isam.KeyDef.KeySegment, null).empty;
        defer {
            for (key_segments_storage.items) |segs| {
                self.allocator.free(segs);
            }
            key_segments_storage.deinit(self.allocator);
        }

        var key_idx: usize = 0;
        var arg_idx: usize = 3;
        while (key_idx < num_keys and arg_idx < args.len) : ({
            key_idx += 1;
            arg_idx += 1;
        }) {
            const key_spec_val = try self.evaluateExpression(args[arg_idx]);
            const key_spec = try key_spec_val.toString(self.allocator);
            defer self.allocator.free(key_spec);

            // Parse key specification string: "START=pos, LENGTH=len[, NAME=name][, TYPE=type]"
            const parsed = try self.parseKeySpec(key_spec, @intCast(key_idx));
            key_segments_storage.append(self.allocator, parsed.segments) catch return RuntimeError.OutOfMemory;
            key_defs.append(self.allocator, .{
                .segments = parsed.segments,
                .allow_duplicates = parsed.allow_dups,
                .changes_allowed = parsed.modifiable,
                .key_number = @intCast(key_idx),
            }) catch return RuntimeError.OutOfMemory;
        }

        if (key_defs.items.len == 0) return RuntimeError.InvalidOperation;

        // Create the ISAM file
        const isam_file = isam.IsamFile.create(
            self.allocator,
            file_spec,
            key_defs.items,
            rec_size,
            .{},
        ) catch return RuntimeError.FileNotOpen;

        // Close it immediately (ISAMC just creates, doesn't keep open)
        isam_file.close();
    }

    const ParsedKeySpec = struct {
        segments: []isam.KeyDef.KeySegment,
        allow_dups: bool,
        modifiable: bool,
    };

    /// Parse a key specification string like "START=1, LENGTH=8, TYPE=ALPHA"
    fn parseKeySpec(self: *Self, spec: []const u8, key_num: u8) RuntimeError!ParsedKeySpec {
        _ = key_num;
        var start: u32 = 0;
        var length: u32 = 0;
        var key_type: isam.KeyType = .alpha;
        var allow_dups = false;
        var modifiable = false;

        // Simple parser for key=value pairs
        var iter = std.mem.splitScalar(u8, spec, ',');
        while (iter.next()) |part| {
            const trimmed = std.mem.trim(u8, part, " \t");
            if (std.mem.indexOf(u8, trimmed, "=")) |eq_pos| {
                const key = std.mem.trim(u8, trimmed[0..eq_pos], " \t");
                const value = std.mem.trim(u8, trimmed[eq_pos + 1 ..], " \t");

                const key_buf = self.allocator.alloc(u8, key.len) catch return RuntimeError.OutOfMemory;
                defer self.allocator.free(key_buf);
                const key_lower = std.ascii.lowerString(key_buf, key);

                if (std.mem.eql(u8, key_lower, "start")) {
                    start = @intCast(std.fmt.parseInt(u32, value, 10) catch 0);
                    if (start > 0) start -= 1; // Convert 1-based to 0-based
                } else if (std.mem.eql(u8, key_lower, "length")) {
                    length = @intCast(std.fmt.parseInt(u32, value, 10) catch 0);
                } else if (std.mem.eql(u8, key_lower, "type")) {
                    const val_buf = self.allocator.alloc(u8, value.len) catch return RuntimeError.OutOfMemory;
                    defer self.allocator.free(val_buf);
                    const val_lower = std.ascii.lowerString(val_buf, value);

                    if (std.mem.eql(u8, val_lower, "nocase")) {
                        key_type = .nocase;
                    } else if (std.mem.eql(u8, val_lower, "decimal")) {
                        key_type = .decimal;
                    } else if (std.mem.eql(u8, val_lower, "integer")) {
                        key_type = .integer;
                    }
                }
            } else {
                // Handle flags without values
                const flag_buf = self.allocator.alloc(u8, trimmed.len) catch return RuntimeError.OutOfMemory;
                defer self.allocator.free(flag_buf);
                const flag_lower = std.ascii.lowerString(flag_buf, trimmed);

                if (std.mem.eql(u8, flag_lower, "dups")) {
                    allow_dups = true;
                } else if (std.mem.eql(u8, flag_lower, "modify")) {
                    modifiable = true;
                }
            }
        }

        if (length == 0) return RuntimeError.InvalidOperation;

        // Create segment
        const segments = self.allocator.alloc(isam.KeyDef.KeySegment, 1) catch
            return RuntimeError.OutOfMemory;
        segments[0] = .{
            .start = start,
            .length = length,
            .key_type = key_type,
        };

        return .{
            .segments = segments,
            .allow_dups = allow_dups,
            .modifiable = modifiable,
        };
    }

    fn executeOpen(self: *Self, open: ast.OpenStatement) RuntimeError!void {
        var channel_num = (try self.evaluateExpression(open.channel)).toInteger();

        // Auto-assign channel if 0
        if (channel_num == 0) {
            // Find first available channel (start from 1)
            var i: i64 = 1;
            while (i < 1024) : (i += 1) {
                if (self.channels[@intCast(i)].mode == .closed) {
                    channel_num = i;
                    // Update the variable with assigned channel
                    switch (open.channel) {
                        .identifier => |name| {
                            self.putVariable(name, Value{ .integer = channel_num }) catch return RuntimeError.OutOfMemory;
                        },
                        else => {},
                    }
                    break;
                }
            }
            if (channel_num == 0) return RuntimeError.InvalidOperation; // No free channels
        }

        if (channel_num < 0 or channel_num >= 1024) return RuntimeError.InvalidOperation;

        const filename = try (try self.evaluateExpression(open.filename)).toString(self.allocator);
        defer self.allocator.free(filename);

        // Parse mode (e.g., "U:I" for Update ISAM)
        // Strip quotes if present (from string literals)
        const mode_str = blk: {
            if (open.mode.len >= 2 and (open.mode[0] == '"' or open.mode[0] == '\'')) {
                break :blk open.mode[1 .. open.mode.len - 1];
            }
            break :blk open.mode;
        };

        const mode: Channel.FileMode = if (std.mem.startsWith(u8, mode_str, "I"))
            .input
        else if (std.mem.startsWith(u8, mode_str, "O"))
            .output
        else if (std.mem.startsWith(u8, mode_str, "U"))
            .update
        else if (std.mem.startsWith(u8, mode_str, "A"))
            .append
        else
            .input;

        // Check for terminal device "tt:"
        if (std.mem.eql(u8, filename, "tt:") or std.mem.eql(u8, filename, "TT:")) {
            self.channels[@intCast(channel_num)] = .{
                .file = null,
                .isam_file = null,
                .mode = mode,
                .current_record = null,
                .is_locked = false,
                .is_terminal = true,
                .key_of_reference = 0,
            };
            return;
        }

        // Check for ISAM mode (":I" suffix in mode string)
        const is_isam = std.mem.indexOf(u8, mode_str, ":I") != null or
            std.mem.indexOf(u8, mode_str, ":i") != null;

        if (is_isam) {
            // Open existing ISAM file (use ISAMC to create)
            const isam_file = isam.IsamFile.open(self.allocator, filename, .read_write) catch {
                return RuntimeError.FileNotOpen;
            };

            self.channels[@intCast(channel_num)] = .{
                .file = null,
                .isam_file = isam_file,
                .mode = mode,
                .current_record = null,
                .is_locked = false,
                .is_terminal = false,
                .key_of_reference = 0,
            };
        } else {
            // Standard file I/O
            const file = std.fs.cwd().openFile(filename, .{
                .mode = switch (mode) {
                    .input => .read_only,
                    .output, .append => .write_only,
                    .update => .read_write,
                    else => .read_only,
                },
            }) catch return RuntimeError.FileNotOpen;

            self.channels[@intCast(channel_num)] = .{
                .file = file,
                .isam_file = null,
                .mode = mode,
                .current_record = null,
                .is_locked = false,
                .is_terminal = false,
                .key_of_reference = 0,
            };
        }
    }

    fn executeClose(self: *Self, close: ast.CloseStatement) RuntimeError!void {
        const channel_num = (try self.evaluateExpression(close.channel)).toInteger();
        if (channel_num < 0 or channel_num >= 1024) return RuntimeError.InvalidOperation;

        var channel = &self.channels[@intCast(channel_num)];

        // Close regular file if open
        if (channel.file) |*f| {
            f.close();
            channel.file = null;
        }

        // Close ISAM file if open
        if (channel.isam_file) |isam_file| {
            isam_file.close();
            channel.isam_file = null;
        }

        // Free current record buffer
        if (channel.current_record) |rec| {
            self.allocator.free(rec);
            channel.current_record = null;
        }

        channel.mode = .closed;
    }

    fn executeStore(self: *Self, store: ast.StoreStatement) RuntimeError!void {
        const channel_num = (try self.evaluateExpression(store.channel)).toInteger();
        if (channel_num < 0 or channel_num >= 1024) return RuntimeError.InvalidOperation;

        const channel = &self.channels[@intCast(channel_num)];

        // Get record data from expression
        const record_value = try self.evaluateExpression(store.record);
        defer {
            // Free the alpha buffer if it was allocated by buildRecordBuffer
            if (record_value == .alpha) {
                self.allocator.free(record_value.alpha);
            }
        }
        const record_data = try record_value.toString(self.allocator);
        defer self.allocator.free(record_data);

        // Check for GETRFA qualifier (get the record's ULID into a variable)
        var getrfa_var: ?[]const u8 = null;
        for (store.qualifiers) |qual| {
            if (std.ascii.eqlIgnoreCase(qual.name, "GETRFA")) {
                if (qual.value) |v| {
                    if (v == .identifier) {
                        getrfa_var = v.identifier;
                    }
                }
            }
        }

        if (channel.isam_file) |isam_file| {
            // ISAM store - generates ULID automatically
            _ = isam_file.store(record_data) catch |err| {
                return switch (err) {
                    isam.IsamError.DuplicateKey => RuntimeError.InvalidOperation,
                    isam.IsamError.IoError => RuntimeError.FileNotOpen,
                    else => RuntimeError.InvalidOperation,
                };
            };

            // If GETRFA was specified, store the ULID into the variable
            if (getrfa_var) |var_name| {
                if (isam_file.getRecordUlidString()) |ulid_str| {
                    // Store the 26-character ULID string into the variable
                    const ulid_slice = self.allocator.alloc(u8, 26) catch return RuntimeError.OutOfMemory;
                    @memcpy(ulid_slice, &ulid_str);
                    try self.putVariable(var_name, Value{ .alpha = ulid_slice });
                }
            }
        } else if (channel.file) |file| {
            // Regular file write (append record)
            _ = file.write(record_data) catch return RuntimeError.FileNotOpen;
            _ = file.write("\n") catch return RuntimeError.FileNotOpen;
        } else {
            return RuntimeError.FileNotOpen;
        }
    }

    fn executeRead(self: *Self, read_stmt: ast.ReadStatement) RuntimeError!void {
        const channel_num = (try self.evaluateExpression(read_stmt.channel)).toInteger();
        if (channel_num < 0 or channel_num >= 1024) return RuntimeError.InvalidOperation;

        const channel = &self.channels[@intCast(channel_num)];

        if (channel.isam_file) |isam_file| {
            // Allocate record buffer
            var record_buf: [4096]u8 = undefined;

            // Check for RFA qualifier - read by ULID instead of key
            var rfa_read = false;
            var getrfa_var: ?[]const u8 = null;
            for (read_stmt.qualifiers) |qual| {
                if (std.ascii.eqlIgnoreCase(qual.name, "RFA")) {
                    rfa_read = true;
                } else if (std.ascii.eqlIgnoreCase(qual.name, "GETRFA")) {
                    if (qual.value) |v| {
                        if (v == .identifier) {
                            getrfa_var = v.identifier;
                        }
                    }
                }
            }

            if (rfa_read and read_stmt.key != null) {
                // RFA read - key contains ULID string
                const key_value = try self.evaluateExpression(read_stmt.key.?);
                const ulid_str = try key_value.toString(self.allocator);
                defer self.allocator.free(ulid_str);

                const len = isam_file.readByUlidString(ulid_str, &record_buf, .{}) catch |err| {
                    return switch (err) {
                        isam.IsamError.InvalidUlid => RuntimeError.InvalidOperation,
                        isam.IsamError.UlidNotFound => RuntimeError.KeyNotFound,
                        else => RuntimeError.FileNotOpen,
                    };
                };

                // Store record in target variable
                try self.storeRecordValue(read_stmt.record, record_buf[0..len]);

                // If GETRFA was specified on read, store the ULID into the variable
                if (getrfa_var) |var_name| {
                    if (isam_file.getRecordUlidString()) |ulid_str_out| {
                        const ulid_slice = self.allocator.alloc(u8, 26) catch return RuntimeError.OutOfMemory;
                        @memcpy(ulid_slice, &ulid_str_out);
                        try self.putVariable(var_name, Value{ .alpha = ulid_slice });
                    }
                }
            } else if (read_stmt.key) |key_expr| {
                // Keyed read (READ) - this establishes the key of reference
                const key_value = try self.evaluateExpression(key_expr);
                const key_data = try key_value.toString(self.allocator);
                defer self.allocator.free(key_data);

                // Parse qualifiers for match mode and KEYNUM
                // KEYNUM sets the key of reference for this channel
                // SynergyDE default match mode is Q_GEQ (greater or equal)
                var match_mode: isam.MatchMode = .greater_equal;
                var key_number: u8 = channel.key_of_reference; // Default to current
                for (read_stmt.qualifiers) |qual| {
                    if (std.ascii.eqlIgnoreCase(qual.name, "KEYNUM")) {
                        if (qual.value) |v| {
                            key_number = @intCast((try self.evaluateExpression(v)).toInteger());
                        }
                    } else if (std.ascii.eqlIgnoreCase(qual.name, "MATCH")) {
                        // Q_EQ, Q_GEQ, Q_GTR, Q_PARTIAL
                        if (qual.value) |v| {
                            const mode_str = try (try self.evaluateExpression(v)).toString(self.allocator);
                            defer self.allocator.free(mode_str);
                            if (std.mem.eql(u8, mode_str, "Q_EQ")) {
                                match_mode = .exact;
                            } else if (std.mem.eql(u8, mode_str, "Q_GEQ")) {
                                match_mode = .greater_equal;
                            } else if (std.mem.eql(u8, mode_str, "Q_GTR")) {
                                match_mode = .greater;
                            } else if (std.mem.eql(u8, mode_str, "Q_PARTIAL")) {
                                match_mode = .partial;
                            }
                        }
                    }
                }

                // Set the channel's key of reference (for subsequent READS)
                channel.key_of_reference = key_number;

                const len = isam_file.read(key_data, &record_buf, .{
                    .key_number = key_number,
                    .match_mode = match_mode,
                }) catch |err| {
                    return switch (err) {
                        isam.IsamError.KeyNotFound => RuntimeError.KeyNotFound,
                        isam.IsamError.EndOfFile => RuntimeError.KeyNotFound,
                        else => RuntimeError.FileNotOpen,
                    };
                };

                // Store record in target variable
                try self.storeRecordValue(read_stmt.record, record_buf[0..len]);

                // If GETRFA was specified on read, store the ULID into the variable
                if (getrfa_var) |var_name| {
                    if (isam_file.getRecordUlidString()) |ulid_str| {
                        const ulid_slice = self.allocator.alloc(u8, 26) catch return RuntimeError.OutOfMemory;
                        @memcpy(ulid_slice, &ulid_str);
                        try self.putVariable(var_name, Value{ .alpha = ulid_slice });
                    }
                }
            } else {
                // Sequential read (READS) - uses channel's key of reference
                // SynergyDE: READS does not have KEYNUM, it uses the key established by READ/FIND
                // For backward compatibility, still check for KEYNUM but prefer channel state
                var key_number: u8 = channel.key_of_reference;

                // Backward compatibility: allow KEYNUM on READS (non-standard)
                for (read_stmt.qualifiers) |qual| {
                    if (std.ascii.eqlIgnoreCase(qual.name, "KEYNUM")) {
                        if (qual.value) |v| {
                            key_number = @intCast((try self.evaluateExpression(v)).toInteger());
                            // Also update channel's key of reference for consistency
                            channel.key_of_reference = key_number;
                        }
                    }
                }

                const len = isam_file.readNextByKey(key_number, &record_buf) catch |err| {
                    return switch (err) {
                        isam.IsamError.EndOfFile => RuntimeError.KeyNotFound,
                        else => RuntimeError.FileNotOpen,
                    };
                };

                try self.storeRecordValue(read_stmt.record, record_buf[0..len]);

                // If GETRFA was specified on READS, store the ULID into the variable
                if (getrfa_var) |var_name| {
                    if (isam_file.getRecordUlidString()) |ulid_str| {
                        const ulid_slice = self.allocator.alloc(u8, 26) catch return RuntimeError.OutOfMemory;
                        @memcpy(ulid_slice, &ulid_str);
                        try self.putVariable(var_name, Value{ .alpha = ulid_slice });
                    }
                }
            }
        } else if (channel.file) |file| {
            // Regular file read
            var record_buf: [4096]u8 = undefined;
            const bytes_read = file.read(&record_buf) catch return RuntimeError.FileNotOpen;
            if (bytes_read == 0) return RuntimeError.KeyNotFound; // EOF

            try self.storeRecordValue(read_stmt.record, record_buf[0..bytes_read]);
        } else {
            return RuntimeError.FileNotOpen;
        }
    }

    fn executeWrite(self: *Self, write_stmt: ast.WriteStatement) RuntimeError!void {
        const channel_num = (try self.evaluateExpression(write_stmt.channel)).toInteger();
        if (channel_num < 0 or channel_num >= 1024) return RuntimeError.InvalidOperation;

        const channel = &self.channels[@intCast(channel_num)];

        // Get record data from expression
        const record_value = try self.evaluateExpression(write_stmt.record);
        const record_data = try record_value.toString(self.allocator);
        defer self.allocator.free(record_data);

        if (channel.isam_file) |isam_file| {
            // ISAM write (update current record)
            isam_file.write(record_data) catch |err| {
                return switch (err) {
                    isam.IsamError.KeyNotFound => RuntimeError.KeyNotFound,
                    else => RuntimeError.FileNotOpen,
                };
            };
        } else if (channel.file) |file| {
            // Regular file write
            _ = file.write(record_data) catch return RuntimeError.FileNotOpen;
        } else {
            return RuntimeError.FileNotOpen;
        }
    }

    fn executeDelete(self: *Self, delete_stmt: ast.DeleteStatement) RuntimeError!void {
        const channel_num = (try self.evaluateExpression(delete_stmt.channel)).toInteger();
        if (channel_num < 0 or channel_num >= 1024) return RuntimeError.InvalidOperation;

        const channel = &self.channels[@intCast(channel_num)];

        if (channel.isam_file) |isam_file| {
            isam_file.delete() catch |err| {
                return switch (err) {
                    isam.IsamError.KeyNotFound => RuntimeError.KeyNotFound,
                    else => RuntimeError.FileNotOpen,
                };
            };
        } else {
            return RuntimeError.InvalidOperation; // DELETE only valid for ISAM
        }
    }

    /// Helper to store record data into a variable or record fields
    fn storeRecordValue(self: *Self, target: ast.Expression, data: []const u8) RuntimeError!void {
        switch (target) {
            .identifier => |name| {
                // Check if it's a record name - if so, update individual fields
                if (self.records.get(name)) |record| {
                    var offset: usize = 0;
                    for (record.fields) |field| {
                        const field_size = self.getFieldSize(field.data_type);
                        const end = @min(offset + field_size, data.len);

                        if (offset < data.len) {
                            // Get current field value (to free it if alpha)
                            if (self.variables.get(field.name)) |current| {
                                switch (current) {
                                    .alpha => |a| self.allocator.free(a),
                                    else => {},
                                }
                            }

                            // Copy field data
                            const src_len = end - offset;
                            const buf = self.allocator.alloc(u8, field_size) catch return RuntimeError.OutOfMemory;
                            @memset(buf, ' ');
                            if (src_len > 0) {
                                @memcpy(buf[0..src_len], data[offset..end]);
                            }
                            try self.variables.put(field.name, Value{ .alpha = buf });
                        }
                        offset += field_size;
                    }
                } else {
                    // Simple variable - copy data to a new buffer
                    const buf = self.allocator.dupe(u8, data) catch return RuntimeError.OutOfMemory;
                    try self.variables.put(name, Value{ .alpha = buf });
                }
            },
            else => {
                // TODO: Handle member access, array indexing, etc.
            },
        }
    }

    /// Evaluate an expression to a value
    pub fn evaluateExpression(self: *Self, expr: ast.Expression) RuntimeError!Value {
        return switch (expr) {
            .integer => |i| Value{ .integer = i },
            .decimal => |d| Value{ .decimal = std.fmt.parseInt(i64, d, 10) catch 0 },
            .string => |s| Value{ .string = blk: {
                // Strip quotes from string literals
                if (s.len >= 2 and (s[0] == '"' or s[0] == '\'')) {
                    break :blk s[1 .. s.len - 1];
                }
                break :blk s;
            } },
            .identifier => |name| blk: {
                // First check if it's a variable
                if (self.variables.get(name)) |val| {
                    break :blk val;
                }
                // Then check if it's a record name - build buffer from fields
                if (self.records.get(name)) |record| {
                    const buf = try self.buildRecordBuffer(record);
                    break :blk Value{ .alpha = buf };
                }
                break :blk Value{ .null_val = {} };
            },
            .binary => |b| try self.evaluateBinary(b),
            .unary => |u| try self.evaluateUnary(u),
            .grouping => |g| try self.evaluateExpression(g.*),
            else => Value{ .null_val = {} },
        };
    }

    /// Build a buffer from record fields
    fn buildRecordBuffer(self: *Self, record: ast.RecordDef) RuntimeError![]u8 {
        // Calculate total size
        var total_size: usize = 0;
        for (record.fields) |field| {
            total_size += self.getFieldSize(field.data_type);
        }

        // Allocate buffer
        const buf = self.allocator.alloc(u8, total_size) catch return RuntimeError.OutOfMemory;
        @memset(buf, ' '); // Initialize with spaces

        // Copy field values into buffer
        var offset: usize = 0;
        for (record.fields) |field| {
            const field_size = self.getFieldSize(field.data_type);
            if (self.variables.get(field.name)) |val| {
                const str = val.toString(self.allocator) catch {
                    continue;
                };
                defer self.allocator.free(str);
                const copy_len = @min(str.len, field_size);
                @memcpy(buf[offset .. offset + copy_len], str[0..copy_len]);
            }
            offset += field_size;
        }

        return buf;
    }

    /// Get the size of a data type
    fn getFieldSize(self: *Self, data_type: ast.DataType) usize {
        _ = self;
        return switch (data_type) {
            .alpha => |a| a.size orelse 1,
            .decimal => |d| d.size orelse 1,
            .implied_decimal => |id| id.total_digits,
            .integer => |i| switch (i) {
                .i1 => 1,
                .i2 => 2,
                .i4 => 4,
                .i8 => 8,
            },
            .string => 256, // Default string size
            else => 8,
        };
    }

    fn evaluateBinary(self: *Self, binary: *const ast.BinaryExpr) RuntimeError!Value {
        const left = try self.evaluateExpression(binary.left);
        const right = try self.evaluateExpression(binary.right);

        return switch (binary.operator) {
            .add => Value{ .integer = left.toInteger() + right.toInteger() },
            .subtract => Value{ .integer = left.toInteger() - right.toInteger() },
            .multiply => Value{ .integer = left.toInteger() * right.toInteger() },
            .divide => blk: {
                const r = right.toInteger();
                if (r == 0) return RuntimeError.DivisionByZero;
                break :blk Value{ .integer = @divTrunc(left.toInteger(), r) };
            },
            .equal => Value{ .boolean = left.toInteger() == right.toInteger() },
            .not_equal => Value{ .boolean = left.toInteger() != right.toInteger() },
            .less_than => Value{ .boolean = left.toInteger() < right.toInteger() },
            .less_equal => Value{ .boolean = left.toInteger() <= right.toInteger() },
            .greater_than => Value{ .boolean = left.toInteger() > right.toInteger() },
            .greater_equal => Value{ .boolean = left.toInteger() >= right.toInteger() },
            .logical_and => Value{ .boolean = left.isTruthy() and right.isTruthy() },
            .logical_or => Value{ .boolean = left.isTruthy() or right.isTruthy() },
            else => Value{ .null_val = {} },
        };
    }

    fn evaluateUnary(self: *Self, unary: *const ast.UnaryExpr) RuntimeError!Value {
        const operand = try self.evaluateExpression(unary.operand);

        return switch (unary.operator) {
            .negate => Value{ .integer = -operand.toInteger() },
            .logical_not => Value{ .boolean = !operand.isTruthy() },
            else => operand,
        };
    }

    fn createDefaultValue(self: *Self, data_type: ast.DataType) RuntimeError!Value {
        return switch (data_type) {
            .alpha => |a| blk: {
                const size = a.size orelse 1;
                const buf = self.allocator.alloc(u8, size) catch return RuntimeError.OutOfMemory;
                @memset(buf, ' ');
                break :blk Value{ .alpha = buf };
            },
            .decimal => Value{ .integer = 0 },
            .implied_decimal => |id| Value{ .implied_decimal = .{
                .value = 0,
                .precision = @intCast(id.precision),
            } },
            .integer => Value{ .integer = 0 },
            .string => Value{ .string = "" },
            else => Value{ .null_val = {} },
        };
    }
};

test "runtime basic execution" {
    const allocator = std.testing.allocator;
    var runtime_inst = Runtime.init(allocator);
    defer runtime_inst.deinit();

    try runtime_inst.variables.put("x", Value{ .integer = 42 });
    const value = runtime_inst.variables.get("x");
    try std.testing.expect(value != null);
    try std.testing.expectEqual(@as(i64, 42), value.?.integer);
}
