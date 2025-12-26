//! Ziggy DBL Bytecode Opcodes
//!
//! Defines the instruction set for the Ziggy virtual machine.

const std = @import("std");

/// Bytecode instruction opcodes
pub const Opcode = enum(u8) {
    // ============================================
    // Stack Operations (0x00-0x0F)
    // ============================================

    nop = 0x00,

    // Push constants
    push_null = 0x01,
    push_true = 0x02,
    push_false = 0x03,
    push_i8 = 0x04, // [i8]
    push_i16 = 0x05, // [i16]
    push_i32 = 0x06, // [i32]
    push_i64 = 0x07, // [i64]
    push_const = 0x08, // [u16 index]
    push_const_wide = 0x09, // [u32 index]

    // Stack manipulation
    pop = 0x0A,
    dup = 0x0B,
    swap = 0x0C,
    rot = 0x0D,

    // ============================================
    // Local Variables (0x10-0x1F)
    // ============================================

    load_local_0 = 0x10,
    load_local_1 = 0x11,
    load_local_2 = 0x12,
    load_local_3 = 0x13,
    load_local = 0x14, // [u8 slot]
    load_local_wide = 0x15, // [u16 slot]

    store_local_0 = 0x16,
    store_local_1 = 0x17,
    store_local_2 = 0x18,
    store_local_3 = 0x19,
    store_local = 0x1A, // [u8 slot]
    store_local_wide = 0x1B, // [u16 slot]

    // ============================================
    // Global Variables (0x20-0x2F)
    // ============================================

    load_global = 0x20, // [u16 index]
    store_global = 0x21, // [u16 index]
    load_common = 0x22, // [u16 common_id, u16 offset]
    store_common = 0x23, // [u16 common_id, u16 offset]

    // ============================================
    // Record/Field Operations (0x30-0x3F)
    // ============================================

    new_record = 0x30, // [u16 type_index]
    free_record = 0x31,

    load_field = 0x32, // [u16 field_index]
    store_field = 0x33, // [u16 field_index]

    load_field_0 = 0x34,
    load_field_1 = 0x35,
    load_field_2 = 0x36,
    store_field_0 = 0x37,
    store_field_1 = 0x38,
    store_field_2 = 0x39,

    load_record_buf = 0x3A,
    store_record_buf = 0x3B,
    clear_record = 0x3C,

    // ============================================
    // Arithmetic (0x40-0x4F)
    // ============================================

    add = 0x40,
    sub = 0x41,
    mul = 0x42,
    div = 0x43,
    mod = 0x44,
    neg = 0x45,

    add_dec = 0x46, // [u8 precision]
    sub_dec = 0x47,
    mul_dec = 0x48,
    div_dec = 0x49,

    incr = 0x4A,
    decr = 0x4B,
    incr_by = 0x4C, // [i32 amount]

    // ============================================
    // Comparison (0x50-0x5F)
    // ============================================

    cmp_eq = 0x50,
    cmp_ne = 0x51,
    cmp_lt = 0x52,
    cmp_le = 0x53,
    cmp_gt = 0x54,
    cmp_ge = 0x55,

    cmp_str_eq = 0x56,
    cmp_str_lt = 0x57,

    log_and = 0x58,
    log_or = 0x59,
    log_not = 0x5A,

    bit_and = 0x5B,
    bit_or = 0x5C,
    bit_xor = 0x5D,
    bit_not = 0x5E,

    // ============================================
    // Control Flow (0x60-0x6F)
    // ============================================

    jump = 0x60, // [i16 offset]
    jump_wide = 0x61, // [i32 offset]
    jump_if_true = 0x62, // [i16 offset]
    jump_if_false = 0x63, // [i16 offset]
    jump_if_null = 0x64, // [i16 offset]

    jump_eq = 0x65, // [i16 offset]
    jump_ne = 0x66,
    jump_lt = 0x67,
    jump_ge = 0x68,

    loop_start = 0x69,
    loop_end = 0x6A,

    // ============================================
    // Subroutine Calls (0x70-0x7F)
    // ============================================

    call = 0x70, // [u16 routine_index]
    call_external = 0x71, // [u16 import_index]
    call_native = 0x72, // [u16 native_index]
    call_indirect = 0x73,

    ret = 0x74,
    ret_val = 0x75,

    xcall = 0x76, // [u16 routine_name_const, u8 arg_count]

    // ============================================
    // Channel I/O (0x80-0x8F)
    // ============================================

    ch_open = 0x80, // [u8 mode_flags]
    ch_close = 0x81,

    ch_read = 0x82,
    ch_reads = 0x83,
    ch_write = 0x84,
    ch_writes = 0x85,

    ch_display = 0x86, // [u8 arg_count]
    ch_accept = 0x87,

    ch_store = 0x88,
    ch_delete = 0x89,
    ch_unlock = 0x8A,

    ch_get_pos = 0x8B,
    ch_set_pos = 0x8C,

    // ============================================
    // ISAM Operations (0x90-0x9F)
    // ============================================

    isam_create = 0x90, // [u8 num_keys]
    isam_open = 0x91,
    isam_store = 0x92,
    isam_read = 0x93, // [u8 key_num, u8 match_mode]
    isam_reads = 0x94,
    isam_write = 0x95,
    isam_delete = 0x96,
    isam_find = 0x97,
    isam_rewind = 0x98,
    isam_lock = 0x99,
    isam_unlock = 0x9A,

    // ============================================
    // String Operations (0xA0-0xAF)
    // ============================================

    str_concat = 0xA0,
    str_slice = 0xA1, // [u16 start, u16 length]
    str_len = 0xA2,
    str_index = 0xA3,

    str_trim = 0xA4,
    str_ltrim = 0xA5,
    str_upper = 0xA6,
    str_lower = 0xA7,

    str_find = 0xA8,
    str_replace = 0xA9,

    str_pad_left = 0xAA,
    str_pad_right = 0xAB,

    // ============================================
    // Type Conversion (0xB0-0xBF)
    // ============================================

    to_int = 0xB0,
    to_dec = 0xB1, // [u8 precision]
    to_str = 0xB2,
    to_alpha = 0xB3, // [u16 size]
    to_bool = 0xB4,

    widen_i8_i64 = 0xB5,
    widen_i16_i64 = 0xB6,
    widen_i32_i64 = 0xB7,

    // ============================================
    // Built-in Functions (0xC0-0xCF)
    // ============================================

    fn_abs = 0xC0,
    fn_sqrt = 0xC1,
    fn_sin = 0xC2,
    fn_cos = 0xC3,
    fn_tan = 0xC4,
    fn_log = 0xC5,
    fn_log10 = 0xC6,
    fn_exp = 0xC7,
    fn_round = 0xC8, // [u8 precision]
    fn_trunc = 0xC9,

    fn_date = 0xCA,
    fn_time = 0xCB,

    fn_size = 0xCC,
    fn_instr = 0xCD,
    fn_mem = 0xCE,
    fn_error = 0xCF,

    // ============================================
    // Extended Operations (0xF0-0xFF)
    // ============================================

    extended = 0xF0, // [u8 sub_opcode, ...]
    debug_break = 0xF1,
    debug_line = 0xF2, // [u16 line]
    assert = 0xF3,

    halt = 0xFF,

    /// Get the size of operands for this opcode
    pub fn operandSize(self: Opcode) usize {
        return switch (self) {
            .nop, .push_null, .push_true, .push_false => 0,
            .pop, .dup, .swap, .rot => 0,
            .load_local_0, .load_local_1, .load_local_2, .load_local_3 => 0,
            .store_local_0, .store_local_1, .store_local_2, .store_local_3 => 0,
            .load_field_0, .load_field_1, .load_field_2 => 0,
            .store_field_0, .store_field_1, .store_field_2 => 0,
            .add, .sub, .mul, .div, .mod, .neg => 0,
            .incr, .decr => 0,
            .cmp_eq, .cmp_ne, .cmp_lt, .cmp_le, .cmp_gt, .cmp_ge => 0,
            .cmp_str_eq, .cmp_str_lt => 0,
            .log_and, .log_or, .log_not => 0,
            .bit_and, .bit_or, .bit_xor, .bit_not => 0,
            .ret, .ret_val, .halt => 0,
            .ch_close, .ch_read, .ch_write, .ch_writes => 0,
            .ch_reads => 1, // [u8 key_num]
            .ch_store, .ch_delete, .ch_unlock => 0,
            .isam_store, .isam_reads, .isam_write, .isam_delete => 0,
            .isam_rewind, .isam_lock, .isam_unlock => 0,
            .str_concat, .str_len, .str_index => 0,
            .str_trim, .str_ltrim, .str_upper, .str_lower => 0,
            .str_find, .str_replace => 0,
            .to_int, .to_str, .to_bool => 0,
            .fn_abs, .fn_sqrt, .fn_sin, .fn_cos, .fn_tan => 0,
            .fn_log, .fn_log10, .fn_exp, .fn_trunc => 0,
            .fn_date, .fn_time, .fn_size, .fn_instr, .fn_mem, .fn_error => 0,
            .debug_break, .loop_start, .loop_end => 0,
            .new_record, .free_record => 0,
            .load_record_buf => 2, // [u16 type_index]
            .store_record_buf => 2, // [u16 type_index]
            .clear_record, .call_indirect, .ch_get_pos, .ch_set_pos => 0,
            .isam_open, .isam_find => 0,

            .push_i8, .load_local, .store_local => 1,
            .ch_open, .ch_display, .ch_accept => 1,
            .isam_create => 1,
            .add_dec, .sub_dec, .mul_dec, .div_dec => 1,
            .to_dec, .fn_round => 1,
            .extended => 1,

            .push_i16, .load_local_wide, .store_local_wide => 2,
            .load_global, .store_global => 2,
            .load_field, .store_field => 2,
            .push_const, .call, .call_external, .call_native => 2,
            .jump, .jump_if_true, .jump_if_false, .jump_if_null => 2,
            .jump_eq, .jump_ne, .jump_lt, .jump_ge => 2,
            .debug_line, .to_alpha => 2,
            .str_pad_left, .str_pad_right => 2,
            .isam_read => 2,

            .xcall => 3, // [u16, u8]

            .push_i32, .jump_wide, .push_const_wide => 4,
            .load_common, .store_common => 4,
            .incr_by => 4,
            .str_slice => 4,

            .widen_i8_i64, .widen_i16_i64, .widen_i32_i64 => 0,
            .assert => 0,

            .push_i64 => 8,
        };
    }

    /// Get human-readable name
    pub fn name(self: Opcode) []const u8 {
        return @tagName(self);
    }
};

/// Open mode flags for ch_open
pub const OpenModeFlags = packed struct(u8) {
    input: bool = false,
    output: bool = false,
    update: bool = false,
    append: bool = false,
    isam: bool = false,
    create: bool = false,
    _reserved: u2 = 0,
};

/// Match mode for ISAM read
pub const MatchMode = enum(u8) {
    exact = 0,
    greater_equal = 1,
    greater = 2,
    partial = 3,
};

test "opcode operand sizes" {
    const testing = std.testing;

    try testing.expectEqual(@as(usize, 0), Opcode.nop.operandSize());
    try testing.expectEqual(@as(usize, 1), Opcode.push_i8.operandSize());
    try testing.expectEqual(@as(usize, 2), Opcode.push_const.operandSize());
    try testing.expectEqual(@as(usize, 4), Opcode.push_i32.operandSize());
    try testing.expectEqual(@as(usize, 8), Opcode.push_i64.operandSize());
}
