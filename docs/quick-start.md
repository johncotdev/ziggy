# Ziggy DBL Quick Start Guide

Get up and running with Ziggy DBL in minutes.

## Building Ziggy

```bash
cd ~/ziggy
zig build
```

The executable is created at `./zig-out/bin/ziggy`.

## Your First Program

Create a file called `hello.dbl`:

```dbl
; hello.dbl

record
    name        ,a20
endrecord

proc
    name = "World"
    display(1, "Hello, ", name, "!")
end
```

Run it:

```bash
./zig-out/bin/ziggy hello.dbl
```

Output:
```
Hello, World!
```

## Program Structure

Every Ziggy DBL program has:

1. **Records** - Define your data structures
2. **PROC** - Your main code
3. **END** - End of program

```dbl
; 1. Define data
record
    counter     ,i4
    message     ,a50
endrecord

; 2. Main code
proc
    counter = 42
    message = "The answer is"
    display(1, message, " ", counter)
end
```

## Data Types at a Glance

| Type | Example | Description |
|------|---------|-------------|
| a20 | `name ,a20` | 20-character string |
| d10 | `amount ,d10` | 10-digit decimal |
| d8.2 | `price ,d8.2` | Decimal with 2 decimal places |
| i4 | `count ,i4` | 32-bit integer |

## Basic Operations

### Variables

```dbl
name = "Alice"          ; Assign string
amount = 100            ; Assign number
total = price * qty     ; Arithmetic
```

### Conditions

```dbl
if (amount > 0)
    display(1, "Positive")
else
    display(1, "Zero or negative")
```

### Display Output

```dbl
display(1, "Name: ", name)
display(1, "Amount: ", amount)
display(1, "Multiple ", "values ", "concatenated")
```

## Working with Files

### Terminal I/O

```dbl
record
    tt      ,i4         ; Channel variable
endrecord

proc
    open(tt, "O", "tt:")
    display(tt, "Writing to terminal")
    close(tt)
end
```

### ISAM Database

```dbl
record customer
    id      ,a8
    name    ,a30
endrecord

record
    ch      ,i4
    tt      ,i4
endrecord

proc
    open(tt, "O", "tt:")

    ; Create ISAM file
    xcall ISAMC("mydata", 38, 1, "START=1, LENGTH=8, TYPE=ALPHA")

    ; Open and use it
    open(ch, "U:I", "mydata")

    id = "REC00001"
    name = "Test Record"
    store(ch, customer)

    display(tt, "Stored record: ", id)

    close(ch)
    close(tt)
end
```

## Common Patterns

### Loop Through Records

```dbl
; Read all records sequentially
reads(ch, customer)
display(tt, customer)

reads(ch, customer)
display(tt, customer)

; Continue until all records read...
```

### Increment Counter

```dbl
record
    count   ,i4
endrecord

proc
    count = 0
    incr(count)         ; count = 1
    incr(count, 5)      ; count = 6
end
```

### Clear Variables

```dbl
clear(name)             ; Set to spaces
clear(amount)           ; Set to 0
```

## File Locations

After running the ISAM example:

```
mydata.ism              ; Index file (B+ tree)
mydata.is1              ; Data file (records)
```

## Next Steps

- Read the [Language Reference](language-reference.md) for complete documentation
- See the [ISAM Reference](isam-reference.md) for database details
- Check the `examples/` folder for more sample programs

## Example Programs

| File | Description |
|------|-------------|
| examples/hello.dbl | Basic hello world |
| examples/isam_demo.dbl | Complete ISAM demo |

Run any example:

```bash
./zig-out/bin/ziggy examples/isam_demo.dbl
```
