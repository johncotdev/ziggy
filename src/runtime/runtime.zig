//! Ziggy DBL Runtime
//!
//! Executes DBL programs by walking the AST.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const isam = @import("../isam/isam.zig");

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
    channels: [1024]Channel,
    call_stack: std.ArrayListAligned(CallFrame, null),
    output_buffer: std.ArrayListAligned(u8, null),

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
            };
        }

        return .{
            .allocator = allocator,
            .variables = std.StringHashMap(Value).init(allocator),
            .channels = channels,
            .call_stack = .empty,
            .output_buffer = .empty,
        };
    }

    /// Clean up runtime
    pub fn deinit(self: *Self) void {
        // Close all open channels
        for (&self.channels) |*ch| {
            if (ch.file) |*f| {
                f.close();
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

        // Free call stack
        for (self.call_stack.items) |*frame| {
            frame.locals.deinit();
        }
        self.call_stack.deinit(self.allocator);
        self.output_buffer.deinit(self.allocator);
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
            try self.variables.put(field.name, value);
        }
    }

    fn executeAssignment(self: *Self, assignment: ast.Assignment) RuntimeError!void {
        const value = try self.evaluateExpression(assignment.value);

        switch (assignment.target) {
            .identifier => |name| {
                try self.variables.put(name, value);
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
        // Channel 1 is typically stdout in DBL
        const channel_num = try self.evaluateExpression(display.channel);
        _ = channel_num;

        for (display.expressions) |expr| {
            const value = try self.evaluateExpression(expr);
            const str = try value.toString(self.allocator);
            defer self.allocator.free(str);
            self.writeOutput(str);
        }
        self.writeOutput("\n");
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
                    try self.variables.put(name, Value{ .integer = new_val });
                }
            },
            else => {},
        }
    }

    fn executeXCall(self: *Self, xcall: ast.XCallStatement) RuntimeError!void {
        // TODO: Implement built-in subroutine dispatch
        _ = self;
        _ = xcall;
    }

    fn executeOpen(self: *Self, open: ast.OpenStatement) RuntimeError!void {
        const channel_num = (try self.evaluateExpression(open.channel)).toInteger();
        if (channel_num < 0 or channel_num >= 1024) return RuntimeError.InvalidOperation;

        const filename = try (try self.evaluateExpression(open.filename)).toString(self.allocator);
        defer self.allocator.free(filename);

        // Parse mode (e.g., "U:I" for Update ISAM)
        const mode: Channel.FileMode = if (std.mem.startsWith(u8, open.mode, "I"))
            .input
        else if (std.mem.startsWith(u8, open.mode, "O"))
            .output
        else if (std.mem.startsWith(u8, open.mode, "U"))
            .update
        else if (std.mem.startsWith(u8, open.mode, "A"))
            .append
        else
            .input;

        // For now, just use standard file I/O
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
        };
    }

    fn executeClose(self: *Self, close: ast.CloseStatement) RuntimeError!void {
        const channel_num = (try self.evaluateExpression(close.channel)).toInteger();
        if (channel_num < 0 or channel_num >= 1024) return RuntimeError.InvalidOperation;

        var channel = &self.channels[@intCast(channel_num)];
        if (channel.file) |*f| {
            f.close();
            channel.file = null;
        }
        channel.mode = .closed;
    }

    /// Evaluate an expression to a value
    pub fn evaluateExpression(self: *Self, expr: ast.Expression) RuntimeError!Value {
        return switch (expr) {
            .integer => |i| Value{ .integer = i },
            .decimal => |d| Value{ .decimal = std.fmt.parseInt(i64, d, 10) catch 0 },
            .string => |s| Value{ .string = s },
            .identifier => |name| self.variables.get(name) orelse Value{ .null_val = {} },
            .binary => |b| try self.evaluateBinary(b),
            .unary => |u| try self.evaluateUnary(u),
            .grouping => |g| try self.evaluateExpression(g.*),
            else => Value{ .null_val = {} },
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
