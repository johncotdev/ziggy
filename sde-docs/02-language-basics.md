# DBL Language Basics

## Character Set

DBL uses ASCII characters combined into sequences. The language recognizes several character subsets:

| Subset | Characters |
|--------|------------|
| Numeric | 0-9 |
| Alphabetic | A-Z, a-z |
| Alphanumeric | A-Z, a-z, 0-9 |
| Alpha | All printable ASCII |

**Important:** Identifier names and keywords are **case insensitive** in DBL.

## Identifiers

Identifiers name namespaces, classes, methods, and variables.

### Rules
- Begin with an alpha character (or underscore in Synergy .NET)
- Followed by alphanumeric characters, underscores (`_`), or dollar signs (`$`)
- Maximum length: 30 characters (31 on OpenVMS)

### Examples
```dbl
customer_name
CustomerName
cust$id
_internalField    ; .NET only
```

## Literals

### Alpha Literals
Character sequences enclosed in matching quotes (single or double).

```dbl
"Hello, World"
'Single quotes work too'
```

Maximum length: 255 characters.

### Decimal Literals
Numeric sequences with optional `+` or `-` sign.

```dbl
42
-100
+255
```

Maximum: 28 digits.

### Implied-Decimal Literals
Numeric values with decimal point.

```dbl
3.14159
-99.99
```

Maximum: 28 digits before point, 28 digits after (56 total).

### Error Literals
Format `$ERR_mnemonic` for runtime errors.

```dbl
$ERR_EOF        ; End of file
$ERR_KEYNF      ; Key not found
```

## Variables

Variables reference data locations defined in the data division.

### Reference Forms

**Simple Reference:**
```dbl
customer_name
order_total
```

**Subscripted Reference (Arrays):**
```dbl
items(1)
matrix(row, col)
```

**Ranged Reference (Substrings):**
```dbl
name(1:5)       ; First 5 characters
name(3,10)      ; 10 characters starting at position 3
```

### Hierarchical Access
Use periods to access nested structure fields:

```dbl
customer.address.city
order.line_items(1).price
```

## Statement Categories

DBL statements fall into six categories:

| Category | Purpose |
|----------|---------|
| Declarative | Define variables and definitions |
| Data Manipulation | Directly modify data |
| File Manipulation | Access records within files |
| Control | Alter execution flow (loops, transfers) |
| Input/Output | Exchange data with devices |
| Interprogram Communication | Pass data between processes |

## Comments

```dbl
; This is a comment (semicolon to end of line)
```

## Line Continuation

Use `&` at end of line to continue statement:

```dbl
xcall long_subroutine_name(parameter1, &
                           parameter2, &
                           parameter3)
```

## Statement Termination

Statements are typically one per line. No explicit terminator (like `;`) is required.
