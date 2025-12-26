# Ziggy IR Layer

The Intermediate Representation (IR) layer sits between the AST and code generation backends, enabling:

- Multiple backends (bytecode VM, native compilation, Zig transpilation)
- .NET interop via C ABI exports
- Optimization passes
- Debugging and inspection

## Architecture

```
Source (.zbl) → Lexer → Parser → AST → IR Lowering → IR → [Backend]
                                                          ↓
                                              ┌───────────┼───────────┐
                                              ↓           ↓           ↓
                                         Bytecode    Zig Source   C Header
                                            VM        (Native)    (.NET)
```

## IR Module Structure

```
src/ir/
├── ir.zig           # Core IR types (Type, Value, Instruction, Function, Module)
├── lower.zig        # AST to IR lowering
├── printer.zig      # Human-readable IR output for debugging
└── emit_zig.zig     # IR to Zig source with C ABI exports
```

## IR Types

The IR type system maps Zibol types to low-level representations:

| Zibol Type | IR Type | Size | C Type |
|----------|---------|------|--------|
| `,a10` | `alpha(10)` | 10 bytes | `uint8_t*` |
| `,d8` | `decimal(8,0)` | 8 bytes | `int64_t` |
| `,d10.2` | `decimal(10,2)` | 10 bytes | `int64_t` |
| `,i4` | `integer(4)` | 4 bytes | `int32_t` |
| `,p8.2` | `packed_decimal(8,2)` | 5 bytes | `int64_t` |

## Exported Functions for .NET

Functions can be exported with C ABI for .NET P/Invoke:

```zbl
; pricing.zbl - Business logic callable from .NET

function calculate_total, d10.2, "ziggy_calculate_total"
    quantity    ,d6
    unit_price  ,d8.2
    discount    ,d4.2
proc
    freturn quantity * unit_price * (1.0 - discount)
endfunction
```

This generates:

### IR Output

```
; Module: pricing

define export @ziggy_calculate_total(%quantity: d6, %unit_price: d8.2, %discount: d4.2) -> d10.2 {
entry:
    %0 = mul d10.2 %quantity, %unit_price
    %1 = const d4.2 1.00
    %2 = sub d4.2 %1, %discount
    %3 = mul d10.2 %0, %2
    ret %3
}
```

### Generated Zig

```zig
/// Function: calculate_total
pub export fn ziggy_calculate_total(
    quantity: i64,
    unit_price: i64,
    discount: i64,
) callconv(.C) i64 {
    const _v0 = quantity * unit_price;
    const _v1 = 100;  // 1.00 scaled
    const _v2 = _v1 - discount;
    const _v3 = _v0 * _v2 / 100;
    return _v3;
}
```

### C Header

```c
// Generated C header for pricing
#ifndef ZIGGY_PRICING_H
#define ZIGGY_PRICING_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

int64_t ziggy_calculate_total(int64_t quantity, int64_t unit_price, int64_t discount);

#ifdef __cplusplus
}
#endif

#endif
```

### .NET Usage

```csharp
using System.Runtime.InteropServices;

public static class ZiggyPricing
{
    [DllImport("libziggy_pricing")]
    private static extern long ziggy_calculate_total(long quantity, long unitPrice, long discount);

    public static decimal CalculateTotal(int quantity, decimal unitPrice, decimal discount)
    {
        // Scale to integer representation (2 decimal places = *100)
        long qty = quantity;
        long price = (long)(unitPrice * 100);
        long disc = (long)(discount * 100);

        long result = ziggy_calculate_total(qty, price, disc);

        // Convert back to decimal
        return result / 100m;
    }
}
```

## IR Instructions

### Memory Operations

- `alloca type, "name"` - Allocate local variable
- `load ptr` - Load value from memory
- `store value, ptr` - Store value to memory
- `field_ptr record_ptr, index` - Get pointer to record field

### Arithmetic

- `add`, `sub`, `mul`, `div`, `mod` - Binary operations
- `neg` - Unary negation

### Comparison

- `cmp_eq`, `cmp_ne`, `cmp_lt`, `cmp_le`, `cmp_gt`, `cmp_ge`

### Control Flow

- `br label` - Unconditional branch
- `cond_br condition, then_label, else_label` - Conditional branch
- `switch value, cases, default` - Multi-way branch
- `ret [value]` - Return from function

### Function Calls

- `call @function(args...)` - Call internal function
- `xcall @routine(args...)` - Call external routine (XCALL)

### I/O

- `io_open channel, "mode", filename`
- `io_close channel`
- `io_read channel, record, key`
- `io_write channel, record`
- `io_display channel, values...`

## Lowering Process

The `lower.zig` module converts AST to IR:

1. **Record Definitions** → IR RecordType
2. **Function Definitions** → IR Function with export linkage
3. **Subroutine Definitions** → IR Function (void return)
4. **Procedural Code** → Main function

### Example Lowering

Zibol:
```zbl
record
    counter ,d4
endrecord

proc
    counter = counter + 1
    display(tt, counter)
end
```

IR:
```
record anonymous {
    counter: d4 @ 0
}

define @main() -> void {
entry:
    %0 = alloca d4 ; counter
    %1 = load %0
    %2 = const d4 1
    %3 = add d4 %1, %2
    store %3, %0
    %4 = load %0
    io_display %tt, %4
    ret
}
```

## Future Backends

### LLVM Backend

```zig
// Future: src/ir/emit_llvm.zig
const llvm = @cImport(@cInclude("llvm-c/Core.h"));

pub fn emitLLVM(module: *ir.Module) !*llvm.LLVMModuleRef {
    // Translate IR to LLVM IR for native codegen
}
```

### Bytecode Backend

```zig
// Future: src/ir/emit_bytecode.zig
pub fn emitBytecode(module: *ir.Module, allocator: Allocator) ![]u8 {
    // Lower IR to stack-based bytecode for VM execution
}
```
