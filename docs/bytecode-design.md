# Ziggy DBL Bytecode Format Specification

**Version 0.1 Draft**

This document specifies the bytecode format for compiled Ziggy DBL programs.

---

## Table of Contents

1. [Overview](#overview)
2. [Design Goals](#design-goals)
3. [File Formats](#file-formats)
4. [Module Structure](#module-structure)
5. [Instruction Set](#instruction-set)
6. [Constant Pool](#constant-pool)
7. [Type System](#type-system)
8. [Routine Definitions](#routine-definitions)
9. [Linking and Loading](#linking-and-loading)
10. [Debug Information](#debug-information)
11. [Examples](#examples)

---

## Overview

Ziggy bytecode is a stack-based intermediate representation optimized for:
- Fast interpretation
- Compact storage
- Rich debugging support
- Easy serialization

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Ziggy VM                                │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────────────┐ │
│  │  Stack  │  │  Heap   │  │ Globals │  │ Channel Table   │ │
│  │ (values)│  │(records)│  │  (vars) │  │ (file handles)  │ │
│  └─────────┘  └─────────┘  └─────────┘  └─────────────────┘ │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────────┐│
│  │                  Bytecode Interpreter                   ││
│  │   fetch → decode → execute → fetch → decode → ...       ││
│  └─────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
```

---

## Design Goals

1. **Simplicity** - Easy to implement interpreter
2. **Compactness** - Small bytecode size
3. **Speed** - Minimal decode overhead
4. **Debuggability** - Source mapping, variable names
5. **Linkability** - Separate compilation, library support
6. **DBL Semantics** - Native support for records, ISAM, channels

---

## File Formats

### Compiled Module (.zbc)

Single compilation unit (one .dbl source file).

```
Extension: .zbc (Ziggy ByteCode)
Magic:     0x5A 0x42 0x43 0x31 ("ZBC1")
```

### Library Bundle (.zbl)

Collection of modules with linking metadata.

```
Extension: .zbl (Ziggy Bytecode Library)
Magic:     0x5A 0x42 0x4C 0x31 ("ZBL1")
```

### Executable (.zbx)

Fully linked, ready to run.

```
Extension: .zbx (Ziggy Bytecode eXecutable)
Magic:     0x5A 0x42 0x58 0x31 ("ZBX1")
```

---

## Module Structure

### Binary Layout

```
┌────────────────────────────────────────┐
│ Header (32 bytes)                      │
├────────────────────────────────────────┤
│ Section Table                          │
├────────────────────────────────────────┤
│ Constant Pool Section                  │
├────────────────────────────────────────┤
│ Type Section (Records, Groups)         │
├────────────────────────────────────────┤
│ Routine Section                        │
├────────────────────────────────────────┤
│ Code Section                           │
├────────────────────────────────────────┤
│ Export Table                           │
├────────────────────────────────────────┤
│ Import Table                           │
├────────────────────────────────────────┤
│ Debug Section (optional)               │
└────────────────────────────────────────┘
```

### Header Format

```
Offset  Size  Field
------  ----  -----
0       4     Magic ("ZBC1")
4       2     Version major
6       2     Version minor
8       4     Flags
12      4     Section count
16      4     Entry point (routine index, 0xFFFFFFFF if library)
20      4     Source hash (for cache invalidation)
24      8     Reserved
```

### Flags

```
Bit 0:  Has debug info
Bit 1:  Has native extensions
Bit 2:  Requires ISAM runtime
Bit 3:  Is library (no entry point)
Bit 4-31: Reserved
```

### Section Table Entry

```
Offset  Size  Field
------  ----  -----
0       4     Section type
4       4     Offset in file
8       4     Size in bytes
12      4     Entry count
```

### Section Types

```zig
pub const SectionType = enum(u32) {
    constants = 0x0001,
    types = 0x0002,
    routines = 0x0003,
    code = 0x0004,
    exports = 0x0005,
    imports = 0x0006,
    debug = 0x0007,
    native = 0x0008,
};
```

---

## Instruction Set

### Instruction Encoding

Instructions use variable-length encoding:

```
┌─────────┬─────────────────────────────┐
│ Opcode  │ Operands (0-8 bytes)        │
│ 1 byte  │                             │
└─────────┴─────────────────────────────┘
```

### Opcode Categories

| Range | Category |
|-------|----------|
| 0x00-0x0F | Stack Operations |
| 0x10-0x1F | Local Variables |
| 0x20-0x2F | Global Variables |
| 0x30-0x3F | Record/Field Operations |
| 0x40-0x4F | Arithmetic |
| 0x50-0x5F | Comparison |
| 0x60-0x6F | Control Flow |
| 0x70-0x7F | Subroutine Calls |
| 0x80-0x8F | Channel I/O |
| 0x90-0x9F | ISAM Operations |
| 0xA0-0xAF | String Operations |
| 0xB0-0xBF | Type Conversion |
| 0xC0-0xCF | Built-in Functions |
| 0xF0-0xFF | Extended/Reserved |

### Complete Opcode Table

```zig
pub const Opcode = enum(u8) {
    // ============================================
    // Stack Operations (0x00-0x0F)
    // ============================================

    nop = 0x00,           // No operation

    // Push constants
    push_null = 0x01,     // Push null value
    push_true = 0x02,     // Push boolean true
    push_false = 0x03,    // Push boolean false
    push_i8 = 0x04,       // Push i8 immediate: [i8]
    push_i16 = 0x05,      // Push i16 immediate: [i16]
    push_i32 = 0x06,      // Push i32 immediate: [i32]
    push_i64 = 0x07,      // Push i64 immediate: [i64]
    push_const = 0x08,    // Push from constant pool: [u16 index]
    push_const_wide = 0x09, // Push from constant pool: [u32 index]

    // Stack manipulation
    pop = 0x0A,           // Pop and discard top
    dup = 0x0B,           // Duplicate top
    swap = 0x0C,          // Swap top two
    rot = 0x0D,           // Rotate top three (a b c -> b c a)

    // ============================================
    // Local Variables (0x10-0x1F)
    // ============================================

    load_local_0 = 0x10,  // Load local 0 (optimized)
    load_local_1 = 0x11,  // Load local 1
    load_local_2 = 0x12,  // Load local 2
    load_local_3 = 0x13,  // Load local 3
    load_local = 0x14,    // Load local: [u8 slot]
    load_local_wide = 0x15, // Load local: [u16 slot]

    store_local_0 = 0x16, // Store local 0
    store_local_1 = 0x17, // Store local 1
    store_local_2 = 0x18, // Store local 2
    store_local_3 = 0x19, // Store local 3
    store_local = 0x1A,   // Store local: [u8 slot]
    store_local_wide = 0x1B, // Store local: [u16 slot]

    // ============================================
    // Global Variables (0x20-0x2F)
    // ============================================

    load_global = 0x20,   // Load global: [u16 index]
    store_global = 0x21,  // Store global: [u16 index]
    load_common = 0x22,   // Load from COMMON: [u16 common_id, u16 offset]
    store_common = 0x23,  // Store to COMMON: [u16 common_id, u16 offset]

    // ============================================
    // Record/Field Operations (0x30-0x3F)
    // ============================================

    // Record allocation
    new_record = 0x30,    // Allocate record: [u16 type_index]
    free_record = 0x31,   // Free record (if heap allocated)

    // Field access (stack: record_ref -> value)
    load_field = 0x32,    // Load field: [u16 field_index]
    store_field = 0x33,   // Store field: [u16 field_index]

    // Optimized field access for small offsets
    load_field_0 = 0x34,  // Load field 0
    load_field_1 = 0x35,  // Load field 1
    load_field_2 = 0x36,  // Load field 2
    store_field_0 = 0x37, // Store field 0
    store_field_1 = 0x38, // Store field 1
    store_field_2 = 0x39, // Store field 2

    // Record buffer operations
    load_record_buf = 0x3A,  // Get record as byte buffer
    store_record_buf = 0x3B, // Set record from byte buffer
    clear_record = 0x3C,     // Clear all fields to defaults

    // ============================================
    // Arithmetic (0x40-0x4F)
    // ============================================

    add = 0x40,           // a + b
    sub = 0x41,           // a - b
    mul = 0x42,           // a * b
    div = 0x43,           // a / b (integer division)
    mod = 0x44,           // a % b
    neg = 0x45,           // -a

    // Implied decimal operations (preserve precision)
    add_dec = 0x46,       // Decimal add: [u8 precision]
    sub_dec = 0x47,       // Decimal sub: [u8 precision]
    mul_dec = 0x48,       // Decimal mul: [u8 result_precision]
    div_dec = 0x49,       // Decimal div: [u8 result_precision]

    incr = 0x4A,          // Increment by 1
    decr = 0x4B,          // Decrement by 1
    incr_by = 0x4C,       // Increment by n: [i32 amount]

    // ============================================
    // Comparison (0x50-0x5F)
    // ============================================

    cmp_eq = 0x50,        // a == b
    cmp_ne = 0x51,        // a != b
    cmp_lt = 0x52,        // a < b
    cmp_le = 0x53,        // a <= b
    cmp_gt = 0x54,        // a > b
    cmp_ge = 0x55,        // a >= b

    // String comparison
    cmp_str_eq = 0x56,    // String equal
    cmp_str_lt = 0x57,    // String less than

    // Logical
    log_and = 0x58,       // a && b
    log_or = 0x59,        // a || b
    log_not = 0x5A,       // !a

    // Bitwise
    bit_and = 0x5B,       // a & b
    bit_or = 0x5C,        // a | b
    bit_xor = 0x5D,       // a ^ b
    bit_not = 0x5E,       // ~a

    // ============================================
    // Control Flow (0x60-0x6F)
    // ============================================

    jump = 0x60,          // Unconditional: [i16 offset]
    jump_wide = 0x61,     // Unconditional: [i32 offset]
    jump_if_true = 0x62,  // Conditional: [i16 offset]
    jump_if_false = 0x63, // Conditional: [i16 offset]
    jump_if_null = 0x64,  // Conditional: [i16 offset]

    // Optimized comparisons with jump
    jump_eq = 0x65,       // Jump if equal: [i16 offset]
    jump_ne = 0x66,       // Jump if not equal: [i16 offset]
    jump_lt = 0x67,       // Jump if less than: [i16 offset]
    jump_ge = 0x68,       // Jump if greater/equal: [i16 offset]

    // Loop support
    loop_start = 0x69,    // Mark loop start (for optimizer)
    loop_end = 0x6A,      // Mark loop end

    // ============================================
    // Subroutine Calls (0x70-0x7F)
    // ============================================

    call = 0x70,          // Call routine: [u16 routine_index]
    call_external = 0x71, // Call imported: [u16 import_index]
    call_native = 0x72,   // Call native: [u16 native_index]
    call_indirect = 0x73, // Call via handle on stack

    ret = 0x74,           // Return (no value)
    ret_val = 0x75,       // Return with value

    // XCALL
    xcall = 0x76,         // XCALL: [u16 routine_name_const, u8 arg_count]

    // ============================================
    // Channel I/O (0x80-0x8F)
    // ============================================

    ch_open = 0x80,       // OPEN: [u8 mode_flags]
                          // Stack: channel, mode_str, filename
    ch_close = 0x81,      // CLOSE: Stack: channel

    ch_read = 0x82,       // READ: Stack: channel, record, key
    ch_reads = 0x83,      // READS: Stack: channel, record
    ch_write = 0x84,      // WRITE: Stack: channel, record
    ch_writes = 0x85,     // WRITES: Stack: channel, record

    ch_display = 0x86,    // DISPLAY: [u8 arg_count]
                          // Stack: channel, arg1, arg2, ...
    ch_accept = 0x87,     // ACCEPT: Stack: channel, variable

    ch_store = 0x88,      // STORE: Stack: channel, record
    ch_delete = 0x89,     // DELETE: Stack: channel
    ch_unlock = 0x8A,     // UNLOCK: Stack: channel

    ch_get_pos = 0x8B,    // Get file position
    ch_set_pos = 0x8C,    // Set file position

    // ============================================
    // ISAM Operations (0x90-0x9F)
    // ============================================

    isam_create = 0x90,   // ISAMC: [u8 num_keys]
                          // Stack: filename, rec_size, key_specs...
    isam_open = 0x91,     // Open ISAM file
    isam_store = 0x92,    // Store record
    isam_read = 0x93,     // Read by key: [u8 key_num, u8 match_mode]
    isam_reads = 0x94,    // Sequential read
    isam_write = 0x95,    // Update current
    isam_delete = 0x96,   // Delete current
    isam_find = 0x97,     // Find without read
    isam_rewind = 0x98,   // Reset to beginning
    isam_lock = 0x99,     // Lock current record
    isam_unlock = 0x9A,   // Unlock current record

    // ============================================
    // String Operations (0xA0-0xAF)
    // ============================================

    str_concat = 0xA0,    // Concatenate two strings
    str_slice = 0xA1,     // Substring: [u16 start, u16 length]
    str_len = 0xA2,       // Get string length
    str_index = 0xA3,     // Get char at index

    str_trim = 0xA4,      // Trim trailing spaces
    str_ltrim = 0xA5,     // Trim leading spaces
    str_upper = 0xA6,     // To uppercase
    str_lower = 0xA7,     // To lowercase

    str_find = 0xA8,      // Find substring
    str_replace = 0xA9,   // Replace substring

    str_pad_left = 0xAA,  // Pad left: [u16 width]
    str_pad_right = 0xAB, // Pad right: [u16 width]

    // ============================================
    // Type Conversion (0xB0-0xBF)
    // ============================================

    to_int = 0xB0,        // Convert to integer
    to_dec = 0xB1,        // Convert to decimal: [u8 precision]
    to_str = 0xB2,        // Convert to string
    to_alpha = 0xB3,      // Convert to alpha: [u16 size]
    to_bool = 0xB4,       // Convert to boolean

    // Numeric conversions
    widen_i8_i64 = 0xB5,  // i8 -> i64
    widen_i16_i64 = 0xB6, // i16 -> i64
    widen_i32_i64 = 0xB7, // i32 -> i64

    // ============================================
    // Built-in Functions (0xC0-0xCF)
    // ============================================

    fn_abs = 0xC0,        // %ABS
    fn_sqrt = 0xC1,       // %SQRT
    fn_sin = 0xC2,        // %SIN
    fn_cos = 0xC3,        // %COS
    fn_tan = 0xC4,        // %TAN
    fn_log = 0xC5,        // %LOG
    fn_log10 = 0xC6,      // %LOG10
    fn_exp = 0xC7,        // %EXP
    fn_round = 0xC8,      // %ROUND: [u8 precision]
    fn_trunc = 0xC9,      // %TRUNC

    fn_date = 0xCA,       // %DATE
    fn_time = 0xCB,       // %TIME

    fn_size = 0xCC,       // %SIZE
    fn_instr = 0xCD,      // %INSTR
    fn_mem = 0xCE,        // %MEM
    fn_error = 0xCF,      // %ERROR

    // ============================================
    // Extended Operations (0xF0-0xFF)
    // ============================================

    extended = 0xF0,      // Extended opcode: [u8 sub_opcode, ...]
    debug_break = 0xF1,   // Debugger breakpoint
    debug_line = 0xF2,    // Line number: [u16 line]
    assert = 0xF3,        // Assertion check

    halt = 0xFF,          // Stop execution
};
```

---

## Constant Pool

The constant pool stores all literal values and identifiers.

### Constant Entry Format

```
Offset  Size     Field
------  ----     -----
0       1        Tag (constant type)
1       varies   Data
```

### Constant Tags

```zig
pub const ConstantTag = enum(u8) {
    integer = 0x01,       // i64 value
    decimal = 0x02,       // i64 value + u8 precision
    string = 0x03,        // u16 length + bytes
    alpha = 0x04,         // u16 length + bytes (space-padded)
    identifier = 0x05,    // u16 length + bytes (for linking)
    record_name = 0x06,   // Reference to type section
    routine_name = 0x07,  // Reference to routine section
};
```

### Example Constant Pool

```
Index  Tag         Data
-----  ---         ----
0      string      "Hello, World!"
1      integer     42
2      decimal     1500 (precision=2) -> 15.00
3      identifier  "cust_id"
4      alpha       "CUST0001" (size=8)
```

---

## Type System

### Type Definition Format

```
Offset  Size  Field
------  ----  -----
0       2     Type ID
2       1     Type kind (record, group, alias)
3       1     Flags
4       2     Name (constant pool index)
6       2     Field count
8       4     Total size in bytes
12      N     Field definitions
```

### Field Definition

```
Offset  Size  Field
------  ----  -----
0       2     Name (constant pool index)
2       1     Data type
3       1     Flags
4       2     Offset in record
6       2     Size
8       1     Precision (for decimals)
9       1     Array dimension count
10      N     Array dimensions (if any)
```

### Data Type Codes

```zig
pub const DataTypeCode = enum(u8) {
    alpha = 0x01,           // a, aNN
    decimal = 0x02,         // d, dNN
    implied_decimal = 0x03, // dNN.PP
    integer1 = 0x04,        // i1
    integer2 = 0x05,        // i2
    integer4 = 0x06,        // i4
    integer8 = 0x07,        // i8
    structure = 0x08,       // @structure
    handle = 0x09,          // Handle type
    group = 0x0A,           // Nested group
};
```

### Example Type Section

```
Type 0: customer (record)
  Size: 48 bytes
  Fields:
    0: cust_id    alpha[8]    offset=0
    1: cust_name  alpha[30]   offset=8
    2: balance    d10.2       offset=38

Type 1: order (record)
  Size: 64 bytes
  Fields:
    0: order_id   alpha[10]   offset=0
    1: cust_id    alpha[8]    offset=10
    2: amount     d12.2       offset=18
    3: status     alpha[1]    offset=30
```

---

## Routine Definitions

### Routine Entry Format

```
Offset  Size  Field
------  ----  -----
0       2     Name (constant pool index)
2       2     Flags
4       4     Code offset (in code section)
8       4     Code length
12      2     Parameter count
14      2     Local count
16      2     Max stack depth
18      N     Parameter definitions
```

### Routine Flags

```
Bit 0:  Is public (exported)
Bit 1:  Is function (has return value)
Bit 2:  Is subroutine (XCALL target)
Bit 3:  Has variable arguments
Bit 4:  Is entry point (PROC)
```

### Parameter Definition

```
Offset  Size  Field
------  ----  -----
0       2     Name (constant pool index)
2       1     Data type code
3       1     Flags (in/out/inout)
4       2     Default value (constant pool, or 0xFFFF)
```

---

## Linking and Loading

### Export Table Entry

```
Offset  Size  Field
------  ----  -----
0       2     Name (constant pool index)
2       1     Kind (routine, record, global)
3       1     Flags
4       2     Index (in respective section)
```

### Import Table Entry

```
Offset  Size  Field
------  ----  -----
0       2     Name (constant pool index)
2       2     Module name (constant pool index, or 0 for any)
4       1     Kind (routine, record, global)
5       1     Flags (required/optional)
```

### Library Bundle Format (.zbl)

```
┌────────────────────────────────────────┐
│ Library Header                         │
│   Magic: "ZBL1"                        │
│   Version                              │
│   Module count                         │
│   Export count (total)                 │
├────────────────────────────────────────┤
│ Module Directory                       │
│   [name, offset, size] × module_count  │
├────────────────────────────────────────┤
│ Master Export Table                    │
│   [name, module_index, local_index]    │
├────────────────────────────────────────┤
│ Module 0 (.zbc data)                   │
├────────────────────────────────────────┤
│ Module 1 (.zbc data)                   │
├────────────────────────────────────────┤
│ ...                                    │
└────────────────────────────────────────┘
```

### Resolution Process

```
1. Load main module
2. Collect imports
3. For each import:
   a. Search loaded modules
   b. Search library path
   c. Load required module
   d. Resolve symbol
4. Verify all required imports resolved
5. Begin execution
```

---

## Debug Information

### Debug Section Format

```
Offset  Size  Field
------  ----  -----
0       4     Source file name (constant pool)
4       4     Line table offset
8       4     Line table count
12      4     Local var table offset
16      4     Local var table count
```

### Line Table Entry

```
Offset  Size  Field
------  ----  -----
0       4     Code offset
4       2     Line number
6       2     Column number
```

### Local Variable Entry

```
Offset  Size  Field
------  ----  -----
0       2     Name (constant pool index)
2       2     Slot number
4       4     Scope start (code offset)
8       4     Scope end (code offset)
12      1     Data type
```

---

## Examples

### Example 1: Hello World

**Source:**
```dbl
proc
    display(1, "Hello, World!")
end
```

**Bytecode:**
```
Constants:
  0: string "Hello, World!"

Code:
  0000: push_i8 1           ; Channel 1
  0002: push_const 0        ; "Hello, World!"
  0004: ch_display 1        ; 1 argument
  0006: halt
```

### Example 2: Variable Assignment

**Source:**
```dbl
record
    counter     ,i4
    name        ,a20
endrecord

proc
    counter = 42
    name = "Alice"
    incr(counter)
end
```

**Bytecode:**
```
Types:
  0: (anonymous record)
      counter: i4, offset=0
      name: alpha[20], offset=4

Constants:
  0: string "Alice"

Code:
  0000: push_i8 42
  0002: store_global 0      ; counter
  0004: push_const 0        ; "Alice"
  0006: store_global 1      ; name
  0008: load_global 0       ; counter
  000A: incr
  000B: store_global 0      ; counter
  000D: halt
```

### Example 3: ISAM Operations

**Source:**
```dbl
record customer
    cust_id     ,a8
    cust_name   ,a30
endrecord

record
    ch          ,i4
endrecord

proc
    xcall ISAMC("customers", 38, 1, "START=1, LENGTH=8")
    open(ch, "U:I", "customers")
    cust_id = "CUST0001"
    cust_name = "Alice Smith"
    store(ch, customer)
    close(ch)
end
```

**Bytecode:**
```
Types:
  0: customer
      cust_id: alpha[8], offset=0
      cust_name: alpha[30], offset=8
  1: (anonymous)
      ch: i4, offset=0

Constants:
  0: string "customers"
  1: integer 38
  2: string "START=1, LENGTH=8"
  3: string "U:I"
  4: string "CUST0001"
  5: string "Alice Smith"

Code:
  ; xcall ISAMC("customers", 38, 1, "START=1, LENGTH=8")
  0000: push_const 0        ; "customers"
  0002: push_const 1        ; 38
  0004: push_i8 1           ; 1 key
  0006: push_const 2        ; key spec
  0008: isam_create 1       ; 1 key

  ; open(ch, "U:I", "customers")
  000A: load_global 0       ; ch (will be assigned)
  000C: push_const 3        ; "U:I"
  000E: push_const 0        ; "customers"
  0010: ch_open 0x06        ; flags: update + isam
  0012: store_global 0      ; save assigned channel

  ; cust_id = "CUST0001"
  0014: push_const 4
  0016: store_field 0       ; customer.cust_id

  ; cust_name = "Alice Smith"
  0018: push_const 5
  001A: store_field 1       ; customer.cust_name

  ; store(ch, customer)
  001C: load_global 0       ; ch
  001E: load_record_buf 0   ; customer record type
  0020: isam_store

  ; close(ch)
  0022: load_global 0       ; ch
  0024: ch_close
  0026: halt
```

### Example 4: Conditional

**Source:**
```dbl
record
    score       ,d3
    grade       ,a1
endrecord

proc
    score = 85
    if (score >= 90)
        grade = "A"
    else
        grade = "B"
end
```

**Bytecode:**
```
Constants:
  0: string "A"
  1: string "B"

Code:
  0000: push_i8 85
  0002: store_global 0      ; score

  ; if (score >= 90)
  0004: load_global 0       ; score
  0006: push_i8 90
  0008: cmp_ge
  0009: jump_if_false +8    ; to else branch

  ; then: grade = "A"
  000B: push_const 0
  000D: store_global 1      ; grade
  000F: jump +4             ; skip else

  ; else: grade = "B"
  0011: push_const 1
  0013: store_global 1      ; grade

  0015: halt
```

---

## VM Implementation Sketch

```zig
pub const VM = struct {
    // Execution state
    ip: usize,              // Instruction pointer
    sp: usize,              // Stack pointer
    fp: usize,              // Frame pointer

    // Memory
    stack: [STACK_SIZE]Value,
    globals: []Value,
    heap: std.heap.ArenaAllocator,

    // Runtime resources
    channels: [1024]Channel,
    isam_files: std.AutoHashMap(i64, *IsamFile),

    // Loaded modules
    modules: std.ArrayList(Module),

    pub fn execute(self: *VM, module: *Module) !void {
        self.ip = module.entry_point;

        while (true) {
            const opcode: Opcode = @enumFromInt(module.code[self.ip]);
            self.ip += 1;

            switch (opcode) {
                .push_i8 => {
                    const val = module.code[self.ip];
                    self.ip += 1;
                    self.push(.{ .integer = val });
                },

                .push_const => {
                    const idx = self.readU16(module);
                    self.push(module.constants[idx]);
                },

                .add => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(.{ .integer = a.toInt() + b.toInt() });
                },

                .store_global => {
                    const idx = self.readU16(module);
                    self.globals[idx] = self.pop();
                },

                .ch_display => {
                    const count = module.code[self.ip];
                    self.ip += 1;
                    try self.executeDisplay(count);
                },

                .isam_store => {
                    try self.executeIsamStore();
                },

                .halt => break,

                else => return error.UnknownOpcode,
            }
        }
    }
};
```

---

## Future Extensions

### Planned Opcodes

```zig
// Object-oriented
new_object = 0xD0,        // Instantiate class
invoke_method = 0xD1,     // Call method
load_property = 0xD2,     // Get property
store_property = 0xD3,    // Set property

// Exception handling
try_start = 0xE0,         // Begin try block
try_end = 0xE1,           // End try block
throw = 0xE2,             // Throw exception
catch = 0xE3,             // Catch handler

// Async
async_call = 0xE8,        // Async routine call
await = 0xE9,             // Await result
```

### Optimization Flags

```
-O0: No optimization (debug)
-O1: Constant folding, dead code elimination
-O2: Inlining, loop optimization
-O3: Aggressive optimization
```

---

*Ziggy DBL Bytecode Specification v0.1 Draft*
