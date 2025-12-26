# DBL Operators and Expressions

## Arithmetic Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `a + b` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `a * b` |
| `/` | Division | `a / b` |
| `**` | Exponentiation | `a ** 2` |
| `#` | Integer division (truncate) | `a # b` |
| `//` | Modulo (remainder) | `a // b` |

### Examples
```dbl
result = 10 + 5         ; 15
result = 10 - 3         ; 7
result = 4 * 5          ; 20
result = 15 / 4         ; 3.75 (if implied decimal)
result = 15 # 4         ; 3 (integer division)
result = 15 // 4        ; 3 (modulo/remainder)
result = 2 ** 8         ; 256
```

### Unary Operators
```dbl
value = -amount         ; Negation
value = +amount         ; Positive (no-op)
```

## Comparison Operators

| Operator | Description |
|----------|-------------|
| `.EQ.` or `==` | Equal to |
| `.NE.` or `!=` | Not equal to |
| `.LT.` or `<` | Less than |
| `.LE.` or `<=` | Less than or equal |
| `.GT.` or `>` | Greater than |
| `.GE.` or `>=` | Greater than or equal |

### Examples
```dbl
if (age >= 18)
if (status .EQ. "A")
if (count != 0)
if (name .LT. "M")      ; Alphabetic comparison
```

## Logical Operators

| Operator | Description |
|----------|-------------|
| `.AND.` or `&&` | Logical AND |
| `.OR.` or `\|\|` | Logical OR |
| `.NOT.` or `!` | Logical NOT |
| `.XOR.` | Exclusive OR |

### Examples
```dbl
if (age >= 18 .AND. status == "A")
if (type == 1 .OR. type == 2)
if (.NOT. found)
if (!found)
```

### Short-Circuit Evaluation
```dbl
; Second condition not evaluated if first is false
if (ptr != ^null && ptr.Value > 0)
```

## Bitwise Operators

| Operator | Description |
|----------|-------------|
| `.BAND.` | Bitwise AND |
| `.BOR.` | Bitwise OR |
| `.BNOT.` | Bitwise NOT |
| `.BXOR.` | Bitwise XOR |
| `<<` | Left shift |
| `>>` | Right shift |

### Examples
```dbl
flags = flags .BAND. mask       ; Mask bits
flags = flags .BOR. new_flag    ; Set bit
flags = flags .BXOR. toggle     ; Toggle bit
result = value << 2             ; Multiply by 4
result = value >> 1             ; Divide by 2
```

## String Operators

### Concatenation
```dbl
full_name = first_name + " " + last_name

; With trimming
full_name = %atrim(first_name) + " " + %atrim(last_name)
```

### Substring (Ranging)
```dbl
first_char = name(1:1)          ; First character
first_five = name(1:5)          ; First 5 characters
last_five = name(26:30)         ; Characters 26-30
middle = name(5,10)             ; 10 chars starting at 5
```

## Assignment Operators

| Operator | Description | Equivalent |
|----------|-------------|------------|
| `=` | Assignment | `a = b` |
| `+=` | Add and assign | `a = a + b` |
| `-=` | Subtract and assign | `a = a - b` |
| `*=` | Multiply and assign | `a = a * b` |
| `/=` | Divide and assign | `a = a / b` |

### Examples
```dbl
count += 1              ; Increment
total -= discount       ; Subtract
balance *= rate         ; Compound
```

## Operator Precedence

From highest to lowest:

1. `()` - Parentheses
2. `**` - Exponentiation
3. `+` `-` (unary) - Positive, Negative
4. `*` `/` `#` `//` - Multiplication, Division
5. `+` `-` - Addition, Subtraction
6. `.LT.` `.LE.` `.GT.` `.GE.` `<` `<=` `>` `>=` - Comparison
7. `.EQ.` `.NE.` `==` `!=` - Equality
8. `.NOT.` `!` - Logical NOT
9. `.AND.` `&&` - Logical AND
10. `.OR.` `||` - Logical OR
11. `.XOR.` - Exclusive OR
12. `=` `+=` `-=` etc. - Assignment

### Use Parentheses for Clarity
```dbl
; Clear precedence
result = (a + b) * c
valid = (age >= 18) .AND. (status == "A")
```

## Type Coercion

### Explicit Type Casting
```dbl
; Force interpretation as specific type
alpha_view = (a)numeric_field
decimal_view = (d)alpha_field
integer_view = (i4)some_field
```

### Implicit Conversions
```dbl
; Numeric to string in concatenation
message = "Total: " + %string(total)

; Decimal to integer (truncates)
int_val = (i4)decimal_val

; Alpha to decimal (parses)
num_val = (d)alpha_num
```

## Conditional Expression

DBL doesn't have a ternary operator like `? :`, but you can use:

```dbl
; Using IF assignment
if (condition) then
    result = value1
else
    result = value2

; Or use CASE for multiple conditions
using status select
(1), result = "Active"
(2), result = "Inactive"
(), result = "Unknown"
endusing
```

## Null Handling

```dbl
; Check for null
if (obj == ^null)
    ; handle null case

; Null-safe member access (in .NET)
if (obj != ^null)
    value = obj.Property
```

## Expression Examples

### Complex Calculations
```dbl
; Calculate compound interest
final = principal * (1 + (rate / 100)) ** years

; Quadratic formula
x1 = (-b + %sqrt(b**2 - 4*a*c)) / (2*a)
```

### String Manipulation
```dbl
; Build formatted string
output = %string(item_no, "ZZZZZ9") + " " + &
         %atrimtosize(description, 30) + " " + &
         %string(price, "ZZZ,ZZ9.99")
```

### Conditional Logic
```dbl
; Complex condition
if ((customer_type == "P" .AND. balance > 10000) .OR. &
    (customer_type == "C" .AND. years_active > 5))
begin
    apply_discount = true
end
```

## Common Built-in Functions

Used in expressions:

| Function | Description |
|----------|-------------|
| `%SIZE(var)` | Size of variable |
| `%LEN(str)` | Length of string |
| `%TRIM(str)` | Trim trailing spaces |
| `%ATRIM(str)` | Trim trailing spaces (alpha) |
| `%STRING(n,fmt)` | Format number to string |
| `%INTEGER(x)` | Convert to integer |
| `%DECIMAL(x)` | Convert to decimal |
| `%ABS(n)` | Absolute value |
| `%SQRT(n)` | Square root |
| `%INSTR(pos,s1,s2)` | Find substring |
| `%UPPER(str)` | Uppercase |
| `%LOWER(str)` | Lowercase |
