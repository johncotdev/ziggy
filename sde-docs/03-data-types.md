# DBL Data Types

## Type Categories

DBL has two type classifications:
- **Value Types** - Store data directly
- **Descriptor Types** - Reference data locations

## Alpha Types

Represent consecutive character sequences limited to printable ASCII.

| Syntax | Description | Default |
|--------|-------------|---------|
| `a` | Parameter/return type | N/A |
| `a*` | Field sized by initial value | Initial value |
| `asize` | Fixed-length field | Spaces |

**Maximum size:** 65,535 characters (32-bit platforms)

### Examples
```dbl
record
    name        ,a30           ; 30 character field
    code        ,a*"ABC"       ; 3 characters, initialized to "ABC"
    buffer      ,a256          ; 256 character buffer
endrecord
```

## Decimal Types

Signed whole numbers stored as ASCII numeric characters.

| Syntax | Description | Default |
|--------|-------------|---------|
| `d` | Parameter/return type | N/A |
| `d*` | Sized by initial value | Initial value |
| `dsize` | Fixed digits | Zeros |

**Range:** 1-28 significant digits

### Storage
- Negative sign does NOT consume storage (encoded in rightmost digit)
- Each digit = 1 byte of storage

### Examples
```dbl
record
    quantity    ,d6            ; 6 digit number (0-999999)
    amount      ,d*123         ; 3 digits, initialized to 123
    counter     ,d10           ; 10 digit number
endrecord
```

## Implied-Decimal Types

Decimal with fractional precision. Decimal point occupies no storage.

| Syntax | Description |
|--------|-------------|
| `d.` | Parameter type |
| `dsize.precision` | Fixed total digits with precision |

**Maximum:** 28 digits whole + 28 digits fractional = 56 total (d56.28)

### Examples
```dbl
record
    price       ,d8.2          ; 8 total digits, 2 decimal (999999.99)
    rate        ,d5.4          ; 5 digits, 4 decimal (9.9999)
    pi          ,d10.9         ; 3.141592653
endrecord
```

## Integer Types

Byte-oriented binary representation of signed whole numbers.

| Type | Bytes | Range |
|------|-------|-------|
| `i1` | 1 | -128 to 127 |
| `i2` | 2 | -32,768 to 32,767 |
| `i4` | 4 | -2,147,483,648 to 2,147,483,647 |
| `i8` | 8 | -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807 |

### Aliases
| Alias | Traditional | .NET |
|-------|-------------|------|
| `int` | i4 | System.Int32 |
| `long` | i8 | System.Int64 |
| `short` | i2 | System.Int16 |
| `sbyte` | i1 | System.SByte |

**Warning:** Integers are stored in native byte order. Files with integers are NOT portable between big-endian and little-endian machines unless using the I option.

### Examples
```dbl
record
    handle      ,i4            ; 32-bit integer
    timestamp   ,i8            ; 64-bit integer
    flags       ,i1            ; 8-bit integer
endrecord
```

## Packed Types

Two digits per byte, plus sign byte. NOT supported in Synergy .NET.

| Syntax | Description |
|--------|-------------|
| `p` | Parameter type |
| `psize` | Fixed packed |
| `psize.precision` | Implied-packed |

### Storage Calculation
```
bytes = (digits / 2) + 1
```

### Examples
```dbl
record
    packed_amt  ,p8            ; 8 digits = 5 bytes
    packed_dec  ,p10.2         ; 10 digits, 2 decimal = 6 bytes
endrecord
```

## String Type

Dynamic string, maps to System.String.

**Default value:** `^NULL`

**Maximum safe size for alpha conversion:** 65,535 characters

### Examples
```dbl
record
    message     ,string
    name        ,@System.String
endrecord
```

## Structure Types

Strongly-typed complex data structures.

### Syntax
```dbl
structure address
    street      ,a40
    city        ,a30
    state       ,a2
    zip         ,d5
endstructure

record
    customer_addr   ,address
endrecord
```

## Handle Types

System-defined types for memory and addresses.

| Type | Size | Purpose |
|------|------|---------|
| `D_HANDLE` | i4 | Memory handles |
| `D_ADDR` | i8/i4 | Routine addresses |
| `D_MAXINT` | i8 | Maximum integer |
| `D_NATINT` | i4 | Native integer |
| `D_NATLNG` | i8/i4 | Native long |
| `D_GRFA_TYPE` | a10 | Global RFA |
| `D_RFA_TYPE` | a6 | Record File Address |

## Dynamic Arrays

Support 1-9 dimensions. Default value: `^NULL`

### Syntax
```dbl
[#]type           ; 1 dimension
[#,#]type         ; 2 dimensions
[#,#,#]type       ; 3 dimensions
```

### Examples
```dbl
record
    items       ,[#]i4         ; Dynamic 1D integer array
    matrix      ,[#,#]d10      ; Dynamic 2D decimal array
    objects     ,[#]@MyClass   ; Dynamic array of objects
endrecord
```

## Numeric Parameter Types

For subroutine/function parameters accepting any numeric type.

| Syntax | Accepts |
|--------|---------|
| `n` | decimal, packed, or integer |
| `n.` | implied decimal or implied packed |

### Example
```dbl
subroutine add_numbers
    a       ,n          ; Any numeric
    b       ,n          ; Any numeric
    result  ,n          ; Any numeric
proc
    result = a + b
    xreturn
endsubroutine
```

## Type Coercion

DBL allows explicit type coercion using parenthesized type:

```dbl
(a)numeric_field      ; Treat as alpha
(d)alpha_field        ; Treat as decimal
(i4)some_field        ; Treat as 4-byte integer
```
