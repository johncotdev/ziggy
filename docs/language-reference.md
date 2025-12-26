# Ziggy DBL Language Reference Manual

**Version 0.1.0**

Ziggy DBL is a Zig implementation of the Synergy DBL programming language, featuring a lexer, parser, runtime interpreter, and ISAM database engine.

---

## Table of Contents

1. [Introduction](#introduction)
2. [Program Structure](#program-structure)
3. [Data Types](#data-types)
4. [Variables and Records](#variables-and-records)
5. [Operators and Expressions](#operators-and-expressions)
6. [Control Flow](#control-flow)
7. [File I/O](#file-io)
8. [ISAM Database Operations](#isam-database-operations)
9. [Built-in Functions](#built-in-functions)
10. [XCALL Subroutines](#xcall-subroutines)
11. [Comments](#comments)
12. [Examples](#examples)

---

## Introduction

Ziggy DBL implements the core features of Synergy DBL, a language designed for business data processing with strong support for:

- Fixed-length record processing
- ISAM (Indexed Sequential Access Method) databases
- File I/O operations
- Decimal arithmetic

### Running Ziggy DBL Programs

```bash
ziggy <filename.dbl>
```

---

## Program Structure

A Ziggy DBL program consists of:

1. **Record definitions** - Define data structures
2. **PROC block** - Main executable code
3. **END statement** - Marks program end

### Basic Program Template

```dbl
; Comment describing the program

record mydata
    field1      ,a20
    field2      ,d10
endrecord

proc
    ; Executable statements here
    display(1, "Hello, World!")
end
```

### Keywords

| Keyword | Description |
|---------|-------------|
| RECORD / ENDRECORD | Define a record structure |
| GROUP / ENDGROUP | Define nested field groups |
| PROC | Begin main procedure |
| END | End program |

---

## Data Types

Ziggy DBL supports the following data types:

### Alpha (a)

Character/string data with fixed or variable length.

```dbl
record
    name        ,a30       ; 30-character string
    code        ,a8        ; 8-character string
    variable    ,a*        ; Variable length (in common blocks)
endrecord
```

Alpha fields are space-padded on the right.

### Decimal (d)

Numeric data stored as ASCII digits.

```dbl
record
    quantity    ,d5        ; 5-digit decimal (0-99999)
    amount      ,d10       ; 10-digit decimal
endrecord
```

### Implied Decimal (d.p)

Fixed-point decimal with implicit decimal places.

```dbl
record
    price       ,d8.2      ; 8 total digits, 2 decimal places
    rate        ,d5.4      ; 5 total digits, 4 decimal places
endrecord
```

The value 12345.67 stored in `d8.2` occupies 8 bytes as "01234567".

### Integer (i)

Binary integer types:

| Type | Size | Range |
|------|------|-------|
| i1 | 1 byte | -128 to 127 |
| i2 | 2 bytes | -32,768 to 32,767 |
| i4 | 4 bytes | -2,147,483,648 to 2,147,483,647 |
| i8 | 8 bytes | Full 64-bit range |

```dbl
record
    count       ,i4        ; 32-bit integer
    flags       ,i1        ; 8-bit integer
    channel     ,i4        ; Channel number
endrecord
```

---

## Variables and Records

### Record Definition

Records define the structure of data:

```dbl
record customer
    cust_id     ,a8        ; Primary key
    cust_name   ,a30       ; Customer name
    balance     ,d10.2     ; Account balance
    status      ,a1        ; Status code
endrecord
```

### Anonymous Records

Records without names define standalone variables:

```dbl
record
    counter     ,i4
    filename    ,a255
endrecord
```

### Field Access

Fields are accessed by name:

```dbl
proc
    cust_id = "CUST0001"
    cust_name = "John Smith"
    balance = 1500.50
end
```

### CLEAR Statement

Reset a variable to its default value:

```dbl
clear(counter)          ; Set to 0
clear(cust_name)        ; Set to spaces
```

### INCR Statement

Increment a numeric variable:

```dbl
incr(counter)           ; Add 1
incr(counter, 5)        ; Add 5
```

---

## Operators and Expressions

### Arithmetic Operators

| Operator | Description | Example |
|----------|-------------|---------|
| + | Addition | `a + b` |
| - | Subtraction | `a - b` |
| * | Multiplication | `a * b` |
| / | Division | `a / b` |
| - | Negation (unary) | `-a` |

### Comparison Operators

Both symbolic and keyword forms are supported:

| Symbolic | Keyword | Description |
|----------|---------|-------------|
| == | .EQ. | Equal |
| != | .NE. | Not equal |
| < | .LT. | Less than |
| <= | .LE. | Less than or equal |
| > | .GT. | Greater than |
| >= | .GE. | Greater than or equal |

### Logical Operators

| Symbolic | Keyword | Description |
|----------|---------|-------------|
| && | .AND. | Logical AND |
| \|\| | .OR. | Logical OR |
| ! | .NOT. | Logical NOT |

### Operator Precedence

From highest to lowest:

1. Unary operators (-, !)
2. Multiplicative (*, /)
3. Additive (+, -)
4. Comparison (<, <=, >, >=)
5. Equality (==, !=)
6. Logical AND
7. Logical OR

### Expression Examples

```dbl
result = (a + b) * c
total = price * quantity
is_valid = (status == "A") .AND. (balance > 0)
```

---

## Control Flow

### IF Statement

```dbl
if (condition)
    ; statements when true

if (condition)
    ; statements when true
else
    ; statements when false
```

### Examples

```dbl
if (balance > 0)
    display(tt, "Account has positive balance")

if (status == "A")
    display(tt, "Active")
else
    display(tt, "Inactive")
```

---

## File I/O

### Channels

File operations use numeric channels (0-1023). Channel variables should be declared as `i4`:

```dbl
record
    tt          ,i4        ; Terminal channel
    infile      ,i4        ; Input file channel
    outfile     ,i4        ; Output file channel
endrecord
```

### OPEN Statement

```dbl
open(channel, mode, filename)
```

**Modes:**

| Mode | Description |
|------|-------------|
| "I" | Input (read-only) |
| "O" | Output (write, create/truncate) |
| "U" | Update (read/write) |
| "A" | Append |
| "U:I" | Update ISAM |
| "I:I" | Input ISAM |

**Special Devices:**

| Device | Description |
|--------|-------------|
| "tt:" | Terminal (console) |

### Examples

```dbl
; Open terminal for output
open(tt, "O", "tt:")

; Open ISAM file for update
open(ch, "U:I", "customers")

; Open text file for input
open(infile, "I", "data.txt")
```

### CLOSE Statement

```dbl
close(channel)
```

### DISPLAY Statement

Write formatted output:

```dbl
display(channel, expr1, expr2, ...)
```

Multiple expressions are concatenated:

```dbl
display(tt, "Customer: ", cust_id, " Name: ", cust_name)
display(tt, "Balance: ", balance)
```

### READ Statement

Read with a key (for ISAM files):

```dbl
read(channel, record, key)
```

### READS Statement

Sequential read:

```dbl
reads(channel, record)
```

### WRITE Statement

Update current record:

```dbl
write(channel, record)
```

### STORE Statement

Store a new record (ISAM):

```dbl
store(channel, record)
```

### DELETE Statement

Delete current record (ISAM):

```dbl
delete(channel)
```

---

## ISAM Database Operations

Ziggy DBL includes a complete ISAM database engine with B+ tree indexing.

### File Structure

ISAM files consist of two physical files:

| Extension | Contents |
|-----------|----------|
| .ism | Index file (B+ tree) |
| .is1 | Data file (records) |

### Creating ISAM Files

Use `XCALL ISAMC` to create an ISAM file:

```dbl
xcall ISAMC(filespec, record_size, num_keys, key_spec1, ...)
```

**Parameters:**

| Parameter | Description |
|-----------|-------------|
| filespec | Base filename (without extension) |
| record_size | Total record size in bytes |
| num_keys | Number of key definitions |
| key_spec | Key specification string |

**Key Specification Format:**

```
"START=pos, LENGTH=len, TYPE=type[, DUPS][, MODIFY]"
```

| Field | Description |
|-------|-------------|
| START | Position in record (1-based) |
| LENGTH | Key length in bytes |
| TYPE | ALPHA, NOCASE, DECIMAL, or INTEGER |
| DUPS | Allow duplicate keys |
| MODIFY | Key can be modified |

### Example: Creating an ISAM File

```dbl
record customer
    cust_id     ,a8        ; Key field at position 1
    cust_name   ,a30
    balance     ,d10
endrecord

proc
    ; Record size = 8 + 30 + 10 = 48 bytes
    ; Key: first 8 bytes (cust_id)
    xcall ISAMC("customers", 48, 1, "START=1, LENGTH=8, TYPE=ALPHA")
end
```

### Opening ISAM Files

```dbl
open(channel, "U:I", "customers")   ; Update mode
open(channel, "I:I", "customers")   ; Input (read-only) mode
```

### Storing Records

```dbl
cust_id = "CUST0001"
cust_name = "Alice Smith"
balance = 1500
store(ch, customer)
```

### Reading Records

**By Key:**
```dbl
read(ch, customer, "CUST0001")
```

**Sequential:**
```dbl
reads(ch, customer)
```

### Key Types

| Type | Description |
|------|-------------|
| ALPHA | Case-sensitive character comparison |
| NOCASE | Case-insensitive character comparison |
| DECIMAL | Numeric (ASCII digit) comparison |
| INTEGER | Binary integer comparison |

### Match Modes

When reading by key, the following match modes apply:

| Mode | Description |
|------|-------------|
| Exact | Key must match exactly |
| Greater/Equal | Find key >= specified value |
| Greater | Find key > specified value |
| Partial | Prefix match |

---

## Built-in Functions

Built-in functions use the `%` prefix:

### String Functions

| Function | Description |
|----------|-------------|
| %TRIM(str) | Remove trailing spaces |
| %ATRIM(str) | Remove trailing spaces (alias) |
| %LTRIM(str) | Remove leading spaces |
| %LEN(str) | String length |
| %SIZE(var) | Variable size in bytes |
| %INSTR(start, str, substr) | Find substring position |
| %UPPER(str) | Convert to uppercase |
| %LOWER(str) | Convert to lowercase |

### Numeric Functions

| Function | Description |
|----------|-------------|
| %ABS(n) | Absolute value |
| %INTEGER(n) | Convert to integer |
| %DECIMAL(n) | Convert to decimal |
| %ROUND(n, p) | Round to p decimal places |
| %TRUNC(n) | Truncate to integer |

### Math Functions

| Function | Description |
|----------|-------------|
| %SQRT(n) | Square root |
| %SIN(n) | Sine (radians) |
| %COS(n) | Cosine |
| %TAN(n) | Tangent |
| %LOG(n) | Natural logarithm |
| %LOG10(n) | Base-10 logarithm |
| %EXP(n) | Exponential (e^n) |

### Date/Time Functions

| Function | Description |
|----------|-------------|
| %DATE | Current date (YYYYMMDD) |
| %TIME | Current time (HHMMSS) |

---

## XCALL Subroutines

External subroutines are called with XCALL:

```dbl
xcall routine_name(arg1, arg2, ...)
```

### Built-in Subroutines

#### ISAMC - Create ISAM File

```dbl
xcall ISAMC(filespec, record_size, num_keys, key_spec, ...)
```

Creates a new ISAM file with the specified key definitions.

---

## Comments

Comments begin with a semicolon and continue to end of line:

```dbl
; This is a comment
record
    field1      ,a10       ; Inline comment
endrecord
```

---

## Examples

### Hello World

```dbl
; hello.dbl - Hello World program

record
    message     ,a20
endrecord

proc
    message = "Hello, World!"
    display(1, message)
end
```

### ISAM Database Demo

```dbl
; isam_demo.dbl - ISAM Database Demo

record customer
    cust_id     ,a8
    cust_name   ,a30
    balance     ,d10
endrecord

record
    tt          ,i4        ; Terminal channel
    ch          ,i4        ; ISAM file channel
endrecord

proc
    ; Open terminal for display
    open(tt, "O", "tt:")

    ; Create ISAM file (48 byte records, 1 key)
    xcall ISAMC("customers", 48, 1, "START=1, LENGTH=8, TYPE=ALPHA")

    ; Open for update
    open(ch, "U:I", "customers")

    ; Store records
    cust_id = "CUST0001"
    cust_name = "Alice Smith"
    balance = 1500
    store(ch, customer)
    display(tt, "Stored: ", cust_id)

    cust_id = "CUST0002"
    cust_name = "Bob Johnson"
    balance = 2300
    store(ch, customer)
    display(tt, "Stored: ", cust_id)

    ; Sequential read
    display(tt, "--- Reading records ---")
    reads(ch, customer)
    display(tt, cust_id, " - ", cust_name)

    reads(ch, customer)
    display(tt, cust_id, " - ", cust_name)

    close(ch)
    close(tt)
end
```

### Conditional Logic

```dbl
; conditional.dbl - IF statement example

record
    score       ,d3
    grade       ,a1
endrecord

proc
    score = 85

    if (score >= 90)
        grade = "A"
    else
        if (score >= 80)
            grade = "B"
        else
            if (score >= 70)
                grade = "C"
            else
                grade = "F"

    display(1, "Score: ", score, " Grade: ", grade)
end
```

---

## Appendix: Reserved Keywords

The following keywords are reserved in Ziggy DBL:

```
ACCEPT      ABS         ABSTRACT    ADDHANDLER  AND
ASYNC       AWAIT       BEGIN       CALL        CASE
CATCH       CLASS       CLEAR       CLOSE       COMMON
DECR        DEFAULT     DELEGATE    DELETE      DETACH
DISPLAY     DO          ELSE        END         ENDCLASS
ENDCOMMON   ENDDELEGATE ENDENUM     ENDFOR      ENDGROUP
ENDIF       ENDINTERFACE ENDLITERAL ENDMETHOD  ENDNAMESPACE
ENDPROPERTY ENDRECORD   ENDSTRUCTURE ENDTRY    ENDUSING
ENDWHILE    ENUM        EXIT        EXITLOOP    EXTENDS
FALSE       FINALLY     FOR         FOREACH     FOREVER
FRETURN     FROM        GLOBAL      GOTO        GROUP
IF          IMPLEMENTS  IMPORT      IN          INCR
INIT        INTERFACE   INTERNAL    LITERAL     LOCASE
LOCK        METHOD      MRETURN     NAMESPACE   NEW
NEXTLOOP    NOT         NOTHING     OFFERROR    ONERROR
OPEN        OR          OVERRIDE    PARENT      PRIVATE
PROC        PROPERTY    PROTECTED   PUBLIC      READ
READS       RECORD      REMOVEHANDLER REPEAT   RETURN
SEALED      STATIC      STOP        STORE       STRUCTURE
THEN        THIS        THROW       THRU        TRUE
TRY         UNTIL       UPCASE      USING       VIRTUAL
WAIT        WHILE       WRITE       WRITES      XCALL
XRETURN     XOR
```

---

## Appendix: Differences from Synergy DBL

Ziggy DBL aims for compatibility with Synergy DBL but has some differences:

1. **Subset Implementation** - Not all features are implemented
2. **No OOP Yet** - Classes and methods are recognized but not executed
3. **Simplified Qualifiers** - I/O qualifiers are parsed but not all are honored
4. **No External Routines** - Only built-in XCALL routines are supported
5. **Single-threaded** - No async/await execution yet

---

*Ziggy DBL Language Reference Manual v0.1.0*
*Generated for Ziggy DBL - A Zig Implementation of Synergy DBL*
