# DBL Program Structure

## Basic Program Layout

A DBL program consists of two main divisions:

```
┌─────────────────────────────┐
│     DATA DIVISION           │
│  - Record definitions       │
│  - Variable declarations    │
│  - Constants/literals       │
├─────────────────────────────┤
│         PROC                │  ← Boundary marker
├─────────────────────────────┤
│    PROCEDURE DIVISION       │
│  - Executable statements    │
│  - Program logic            │
├─────────────────────────────┤
│         END                 │  ← Terminator
└─────────────────────────────┘
```

## Simple Program Example

```dbl
; Data Division
record
    name        ,a30
    age         ,d3
    salary      ,d10.2
endrecord

; Procedure Division
proc
    name = "John Smith"
    age = 35
    salary = 75000.00

    display(1, name)

end
```

## RECORD-ENDRECORD

Defines a data structure with fields.

### Syntax
```dbl
record [name]
    field_definitions...
endrecord
```

### Unnamed Record (Local)
```dbl
record
    counter     ,d5
    buffer      ,a256
endrecord
```

### Named Record
```dbl
record customer_rec
    cust_id     ,d8
    cust_name   ,a50
    balance     ,d12.2
endrecord
```

## GROUP-ENDGROUP

Defines a group of related fields within a record.

```dbl
record
    group address
        street      ,a40
        city        ,a30
        state       ,a2
        zip         ,d5
    endgroup

    group phone
        area_code   ,d3
        number      ,d7
    endgroup
endrecord
```

### Accessing Group Fields
```dbl
address.city = "Sydney"
phone.area_code = 02
```

## STRUCTURE-ENDSTRUCTURE

Defines a reusable data layout (like a typedef).

```dbl
structure line_item
    item_code       ,d6
    description     ,a30
    quantity        ,d5
    unit_price      ,d8.2
    line_total      ,d10.2
endstructure

record
    order_lines     ,[10]line_item    ; Array of 10 line items
endrecord
```

## COMMON-ENDCOMMON

Defines shared data accessible across routines.

```dbl
common /shared_data/
    company_name    ,a50
    fiscal_year     ,d4
    tax_rate        ,d5.4
endcommon
```

Common blocks are shared by name across the application.

## GLOBAL-ENDGLOBAL

Defines global data section.

```dbl
global data section globals
    record
        app_version     ,a10
        debug_mode      ,d1
    endrecord
endglobal
```

## LITERAL-ENDLITERAL

Defines named constants.

```dbl
literal
    MAX_ITEMS       ,d5     ,100
    DEFAULT_NAME    ,a10    ,"UNKNOWN"
    PI              ,d10.8  ,3.14159265
endliteral
```

## PROC-END

Marks boundary between data and procedure divisions.

```dbl
record
    ; Data definitions here
endrecord

proc
    ; Executable code here

end
```

### Alternative Endings
- `ENDMAIN` - For main routine
- `ENDSUBROUTINE` - For subroutines
- `ENDFUNCTION` - For functions
- `ENDMETHOD` - For methods

## MAIN-ENDMAIN

Explicitly defines the main entry point.

```dbl
main
record
    result      ,d10
endrecord
proc
    xcall calculate_something(result)
    display(1, "Result: ", result)
endmain
```

## SUBROUTINE-ENDSUBROUTINE

Defines an external subroutine.

```dbl
subroutine process_customer
    cust_id     ,d8         ; Input parameter
    cust_name   ,a50        ; Output parameter
    status      ,d1         ; Output status

record
    ; Local variables
    temp_data   ,a100
endrecord

proc
    ; Subroutine logic
    status = 1
    xreturn

endsubroutine
```

## FUNCTION-ENDFUNCTION

Defines a function that returns a value.

```dbl
function calculate_tax, d10.2
    amount      ,d10.2
    rate        ,d5.4

record
    tax_amount  ,d10.2
endrecord

proc
    tax_amount = amount * (rate / 100)
    freturn tax_amount

endfunction
```

### Usage
```dbl
proc
    tax = %calculate_tax(subtotal, 10.0)
end
```

## Arrays

### Fixed Arrays
```dbl
record
    items       ,[100]a30       ; 100 element array
    matrix      ,[10,10]d8      ; 10x10 matrix
    cube        ,[5,5,5]i4      ; 3D array
endrecord
```

### Array Access
```dbl
items(1) = "First item"         ; 1-based indexing
items(50) = "Middle item"
value = matrix(row, col)
```

## Overlays

Define alternate views of the same memory.

```dbl
record
    full_date       ,d8         ; YYYYMMDD
    group date_parts ,a8 @full_date
        year        ,d4
        month       ,d2
        day         ,d2
    endgroup
endrecord
```

Now `full_date`, `year`, `month`, and `day` share the same 8 bytes.

## Data Initialization

### At Declaration
```dbl
record
    counter     ,d5     ,0
    name        ,a20    ,"Default"
    rate        ,d5.2   ,0.00
endrecord
```

### INIT Statement
```dbl
proc
    init customer_rec           ; Reset to defaults
end
```

### CLEAR Statement
```dbl
proc
    clear name                  ; Set to spaces
    clear counter               ; Set to zero
end
```
