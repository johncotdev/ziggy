# DBL File Types

DBL supports four distinct file types, each with specific characteristics and use cases.

## File Type Overview

| Type | Access Method | Record Length | Use Case |
|------|---------------|---------------|----------|
| ISAM | Keyed + Sequential | Fixed/Variable | Primary database files |
| Relative | Position + Sequential | Fixed | Random access by record number |
| Sequential | Sequential only | Variable | Text files, logs, exports |
| Stream | Random by position | Terminated | Default on Windows/Unix |

## ISAM Files

### Characteristics
- Series of data records referenced by key
- High-speed keyed access maintained as file grows
- Multiple indexes supported (multiple keys)
- Portable across Windows and Unix platforms
- Two physical files: `.ism` (index) and `.is1` (data)

### Creation
```dbl
; Using ISAMC subroutine
xcall isamc(filename, num_keys, key_spec)

; Using bldism utility (command line)
; bldism customer.ism
```

### Opening
```dbl
; Update mode with ISAM access
open(1, U:I, "customer.ism")

; Input only
open(1, I:I, "customer.ism")
```

### Operations
```dbl
; Read by key
read(1, customer_rec, cust_id)

; Store new record
store(1, new_customer)

; Update current record
write(1, customer_rec)

; Delete current record
delete(1)

; Sequential read
reads(1, customer_rec, eof_label)

; Position without reading
find(1, , key_value)
```

### Detailed Coverage
See [08-isam-deep-dive.md](08-isam-deep-dive.md) for comprehensive ISAM documentation.

## Relative Files

### Characteristics
- Fixed-length records referenced by position
- Access by record number (1-based)
- Also supports sequential access
- Portable across platforms

### Creation
Created automatically when opening in output/update mode with R submode.

### Opening
```dbl
; Open relative file
open(1, U:R, "data.rel")
open(2, O:R, "new_data.rel")
```

### Operations
```dbl
; Read by record number
read(1, record_data, record_number)

; Write at specific position
write(1, record_data, record_number)

; Sequential read
reads(1, record_data, eof_label)

; Delete (marks slot as empty)
delete(1)
```

### Use Cases
- When you need random access by known position
- Simple data storage without complex indexing
- When record count is relatively stable

## Sequential Files

### Characteristics
- Records accessed in order
- Variable-length records supported
- No direct access by position
- Common for text files and data exchange

### Opening
```dbl
; Input sequential
open(1, I:S, "input.txt")

; Output sequential
open(2, O:S, "output.txt")

; Append mode
open(3, A:S, "log.txt")
```

### Operations
```dbl
; Read next record
reads(1, line_data, eof_label)

; Write record
writes(2, output_line)
```

### Use Cases
- Text file processing
- Log files
- Data import/export
- Reports

## Stream Files

### Characteristics
- Records defined by terminators
- Default file type on Windows and Unix
- Random access based on data area size
- Flexible for mixed-format data

### Opening
```dbl
; Default on Windows/Unix - stream mode
open(1, O, "output.dat")

; Explicit stream
open(1, O:Stream, "output.dat")
```

### Operations
Similar to sequential but with position capabilities.

## File Modes

### Mode Characters

| Mode | Description |
|------|-------------|
| `I` | Input (read only) |
| `O` | Output (write, creates/truncates) |
| `U` | Update (read/write) |
| `A` | Append (add to end) |

### Submode Characters

| Submode | Description |
|---------|-------------|
| `I` | ISAM access |
| `R` | Relative access |
| `S` | Sequential access |
| (none) | Stream (default on Windows/Unix) |

### Combined Examples
```dbl
open(1, U:I, "file.ism")    ; Update ISAM
open(2, I:S, "file.txt")    ; Input Sequential
open(3, O:R, "file.rel")    ; Output Relative
open(4, A:S, "log.txt")     ; Append Sequential
```

## Common Qualifiers

### SHARE
```dbl
open(1, U:I, "data.ism", SHARE:Q_EXCL_RW)    ; Exclusive
open(1, U:I, "data.ism", SHARE:Q_NO_EXCL)    ; Shared
```

### LOCK
```dbl
read(1, rec, key, LOCK:Q_NO_LOCK)    ; Don't lock
read(1, rec, key, LOCK:Q_AUTO_LOCK)  ; Auto lock
```

### WAIT
```dbl
read(1, rec, key, WAIT:10)           ; Wait 10 seconds for lock
```

### ERROR
```dbl
open(1, I:S, filename, ERROR:not_found)    ; Goto on error
```

## Channel Management

### Channel Numbers
- Channels 1-1024 typically available
- Channel 0 reserved for terminal I/O
- Must open before use, close when done

### Best Practices
```dbl
record
    channel     ,i4
endrecord

proc
    ; Get available channel
    channel = %syn_freechn()

    ; Open file
    open(channel, U:I, "data.ism")

    ; ... process ...

    ; Always close
    close(channel)
end
```

## Error Handling

### Common File Errors

| Error | Meaning |
|-------|---------|
| `$ERR_EOF` | End of file |
| `$ERR_FNF` | File not found |
| `$ERR_LOCKED` | Record locked |
| `$ERR_KEYNF` | Key not found |
| `$ERR_DUP` | Duplicate key |
| `$ERR_NOCURR` | No current record |

### Error Handling Pattern
```dbl
proc
    onerror file_error

    open(1, U:I, "data.ism")
    read(1, record, key)
    ; ... process ...
    close(1)

    offerror
    stop

file_error,
    using %error select
    ($ERR_FNF),
        display(1, "File not found")
    ($ERR_KEYNF),
        display(1, "Key not found")
    ($ERR_LOCKED),
        display(1, "Record locked by another user")
    (),
        display(1, "Unexpected error: ", %error)
    endusing
    stop
end
```

## File Portability

### Portable Files
- ISAM files (without integers)
- Sequential text files
- Relative files (without integers)

### Non-Portable Issues
- Integer data (byte order differs)
- Use the `I` option for portable integers
- Packed decimal is portable
- Alpha and decimal are portable
