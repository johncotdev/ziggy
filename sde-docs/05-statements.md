# DBL Statement Reference

## Control Flow

### IF Statement
```dbl
if (condition)
    statement

if (condition) then
    statement
else
    statement
```

### IF-THEN-ELSE
```dbl
if (age >= 18) then
    display(1, "Adult")
else
    display(1, "Minor")
```

### CASE-ENDCASE
```dbl
case status of
    1: display(1, "Active")
    2: display(1, "Inactive")
    3: display(1, "Pending")
    default: display(1, "Unknown")
endcase
```

### BEGIN-END (Compound Statements)
```dbl
if (condition)
begin
    statement1
    statement2
    statement3
end
```

### USING-ENDUSING
```dbl
using value select
(1),
    display(1, "One")
(2),
    display(1, "Two")
(3, 4, 5),
    display(1, "Three, Four or Five")
endusing
```

### USING-RANGE
```dbl
using score select
(0 thru 59),
    grade = "F"
(60 thru 69),
    grade = "D"
(70 thru 79),
    grade = "C"
(80 thru 89),
    grade = "B"
(90 thru 100),
    grade = "A"
endusing
```

## Loops

### DO FOREVER
```dbl
do forever
begin
    ; Infinite loop - must EXITLOOP or STOP
    if (done)
        exitloop
end
```

### DO-UNTIL
```dbl
do
begin
    process_record()
    read(channel, record, eof)
end
until (eof)
```

### FOR-FROM-THRU
```dbl
for i from 1 thru 10
begin
    display(1, i)
end
```

### FOR-DO
```dbl
for (i = 1; i <= count; i += 1)
begin
    process_item(i)
end
```

### WHILE
```dbl
while (more_records)
begin
    process_record()
    read_next()
end
```

### FOREACH (Collections)
```dbl
foreach item in collection
begin
    display(1, item)
end
```

### Loop Control
```dbl
exitloop                ; Exit innermost loop
exitloop 2              ; Exit 2 nested loops
nextloop                ; Skip to next iteration
```

## File Operations

### OPEN
```dbl
open(channel, mode, filename) [qualifiers]

; Modes: I (input), O (output), U (update), A (append)
open(1, U:I, "customer.ism")        ; Update ISAM
open(2, I:S, "data.txt")            ; Input Sequential
open(3, O:S, "output.txt")          ; Output Sequential
```

### CLOSE
```dbl
close(channel)
close 1
```

### READ (Keyed/Random)
```dbl
read(channel, record, key) [qualifiers]

read(1, customer_rec, cust_id)
read(1, customer_rec, key, KEYNUM:2)        ; Use key 2
read(1, customer_rec, key, LOCK:Q_NO_LOCK)  ; No lock
```

### READS (Sequential)
```dbl
reads(channel, record) [eof_label]

reads(2, line_data, done)           ; Read next, goto done at EOF
```

### WRITE
```dbl
write(channel, record) [qualifiers]

write(1, customer_rec)              ; Update current record
write(1, customer_rec, key)         ; Write at specific key
```

### WRITES (Sequential)
```dbl
writes(channel, data)

writes(3, output_line)              ; Write line to sequential file
```

### STORE (ISAM Insert)
```dbl
store(channel, record) [qualifiers]

store(1, new_customer)              ; Add new ISAM record
```

### DELETE (ISAM)
```dbl
delete(channel)                     ; Delete current record
```

### FIND (Position)
```dbl
find(channel, , key) [qualifiers]

find(1, , cust_id)                  ; Position without reading
```

### UNLOCK
```dbl
unlock(channel)                     ; Release record lock
```

### FLUSH
```dbl
flush(channel)                      ; Flush buffers to disk
```

## Data Manipulation

### CLEAR
```dbl
clear field_name                    ; Alpha → spaces, Numeric → zeros
clear record_name
```

### INIT
```dbl
init structure_name                 ; Reset to declared defaults
```

### INCR / DECR
```dbl
incr counter                        ; counter += 1
incr counter, 5                     ; counter += 5
decr counter                        ; counter -= 1
```

### SET
```dbl
set field1, field2, field3 = value  ; Set multiple at once
```

### LOCASE / UPCASE
```dbl
locase field_name                   ; Convert to lowercase
upcase field_name                   ; Convert to uppercase
```

## I/O Statements

### DISPLAY
```dbl
display(channel, data1, data2, ...)

display(1, "Customer: ", cust_name, " Balance: ", balance)
display(1, 27, "[2J")               ; Clear screen (escape sequence)
```

### ACCEPT
```dbl
accept(channel, field) [wait_time]

accept(1, input_char)               ; Get single character
accept(1, input_char, 5)            ; Wait 5 seconds
```

### GET
```dbl
get(channel, record, length)

get(1, buffer, 256)                 ; Read 256 bytes
```

### PUT
```dbl
put(channel, record, length)

put(1, buffer, 256)                 ; Write 256 bytes
```

### GETS / PUTS (Sequential)
```dbl
gets(channel, buffer, length)
puts(channel, buffer, length)
```

### FORMS (Terminal Control)
```dbl
forms(control_code)

forms(FF)                           ; Form feed
forms(CR)                           ; Carriage return
```

### LPQUE (Print Queue)
```dbl
lpque(filename, queue_name)

lpque("report.txt", "LASER1")
```

## Subroutine/Function Calls

### XCALL (External Call)
```dbl
xcall subroutine_name(arg1, arg2, ...)

xcall process_order(order_id, status)
```

### XRETURN
```dbl
xreturn                             ; Return from subroutine
```

### CALL (Internal)
```dbl
call internal_label

call process_header
; ...
process_header,
    ; internal subroutine code
    return
```

### FRETURN (Function Return)
```dbl
freturn value

freturn calculated_total
```

### MRETURN (Method Return)
```dbl
mreturn [value]

mreturn this
```

## Exception Handling

### TRY-CATCH-FINALLY
```dbl
try
begin
    risky_operation()
end
catch (ex, @Exception)
begin
    display(1, "Error: ", ex.Message)
end
finally
begin
    cleanup()
end
endtry
```

### THROW
```dbl
throw new ApplicationException("Something went wrong")
```

### ONERROR / OFFERROR (Legacy)
```dbl
onerror error_handler

; ... code that might error ...

offerror

error_handler,
    display(1, "Error occurred")
    return
```

## Miscellaneous

### GOTO
```dbl
goto label_name                     ; Transfer control

goto done
```

### STOP
```dbl
stop                                ; Terminate program
stop status_code                    ; Terminate with exit code
```

### SLEEP
```dbl
sleep seconds                       ; Pause execution

sleep 5                             ; Sleep 5 seconds
sleep 0.5                           ; Sleep 500ms
```

### NOP
```dbl
nop                                 ; No operation (placeholder)
```

### DATA (Stack Variable)
```dbl
proc
    data local_var, d10             ; Declare in procedure division
    data temp_str, string
end
```

### YIELD (Iterators)
```dbl
yield mreturn value                 ; Yield from iterator
```

### AWAIT (Async)
```dbl
await async_task                    ; Wait for async completion
```

## I/O Qualifiers

Common qualifiers for file operations:

| Qualifier | Description |
|-----------|-------------|
| `KEYNUM:n` | Specify key number |
| `LOCK:mode` | Lock mode (Q_NO_LOCK, etc.) |
| `MATCH:mode` | Match mode (Q_GEQ, Q_EQ, etc.) |
| `WAIT:seconds` | Wait for lock |
| `POSITION:pos` | File position |
| `SHARE:mode` | Share mode |
| `DIRECTION:dir` | Search direction |
| `RFA:var` | Get record file address |
| `GETRFA:var` | Get RFA after operation |
