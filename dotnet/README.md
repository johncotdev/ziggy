# Ziggy ISAM .NET Interop

This directory contains the .NET bindings for Ziggy ISAM, enabling C# and other .NET languages to access Ziggy DBL ISAM databases.

## Overview

The Ziggy ISAM .NET interop provides:

- **Native Performance**: Direct P/Invoke calls to the Zig-compiled native library
- **Schema Introspection**: Runtime access to table and field definitions stored in the database
- **ULID Support**: Modern 26-character unique identifiers for direct record access
- **Type-Safe Accessors**: Base classes for building strongly-typed record wrappers
- **Multi-Table Support**: Tag-byte discrimination for files with multiple record types

## Architecture

```
┌─────────────────────────────────────────────────┐
│             .NET Application                     │
│  (C#, F#, VB.NET, etc.)                         │
└─────────────────────┬───────────────────────────┘
                      │ P/Invoke
┌─────────────────────▼───────────────────────────┐
│              ZiggyIsam.cs                        │
│  - IsamFile (managed wrapper)                   │
│  - SchemaInfo, TableInfo, FieldInfo             │
│  - RecordAccessor<T> base class                 │
└─────────────────────┬───────────────────────────┘
                      │ C ABI
┌─────────────────────▼───────────────────────────┐
│           libziggy_isam.dylib                   │
│  (or .so on Linux, .dll on Windows)             │
│  - 38 exported C functions                      │
│  - Zig implementation with safety               │
└─────────────────────────────────────────────────┘
```

## Quick Start

### 1. Build the Native Library

```bash
cd /path/to/ziggy
zig build
```

This creates:
- `zig-out/lib/libziggy_isam.dylib` (macOS)
- `zig-out/lib/libziggy_isam.so` (Linux)
- `zig-out/lib/ziggy_isam.dll` (Windows)

### 2. Add to Your .NET Project

Copy `ZiggyIsam.cs` to your project, or reference the `ZiggyIsam.csproj` project.

Ensure the native library is in your application's output directory or system library path.

### 3. Basic Usage

```csharp
using Ziggy.Isam;

// Create a new ISAM file
using var file = IsamFile.Create("customers",
    recordSize: 48,
    keyStart: 0,
    keyLength: 8);

// Store a record
byte[] record = Encoding.ASCII.GetBytes("CUST0001John Smith".PadRight(48));
file.Store(record);

// Read by key
var data = file.Read("CUST0001");
Console.WriteLine(Encoding.ASCII.GetString(data));
```

## API Reference

### IsamFile Class

The main class for ISAM file operations.

#### Creating and Opening Files

```csharp
// Create new file
IsamFile.Create(string filename, uint recordSize, uint keyStart, uint keyLength)

// Open existing file
IsamFile.Open(string filename, OpenMode mode = OpenMode.ReadWrite)
```

#### Record Operations

```csharp
// Store a new record
void Store(byte[] record)

// Store and get ULID
string StoreWithUlid(byte[] record)

// Read by key
ReadOnlySpan<byte> Read(byte[] key, MatchMode mode = MatchMode.Exact)
ReadOnlySpan<byte> Read(string key, MatchMode mode = MatchMode.Exact)

// Read next in sequence
ReadOnlySpan<byte> ReadNext()

// Read by ULID
ReadOnlySpan<byte> ReadByUlid(string ulid)

// Update current record
void Write(byte[] record)

// Delete current record
void Delete()

// Unlock current record
void Unlock()

// Flush to disk
void Flush()
```

#### ULID Access

```csharp
// Get current record's ULID
string? GetCurrentUlid()
```

### Schema Introspection

When a database has an embedded schema, you can inspect its structure at runtime:

```csharp
using var file = IsamFile.Open("orders");

if (file.Schema != null)
{
    Console.WriteLine($"Description: {file.Schema.Description}");
    Console.WriteLine($"Tables: {file.Schema.Tables.Length}");

    foreach (var table in file.Schema.Tables)
    {
        Console.WriteLine($"  Table: {table.Name} (tag: {(char)table.Tag})");
        foreach (var field in table.Fields)
        {
            Console.WriteLine($"    {field.Name}: {field.Type} at {field.Position}");
        }
    }
}
```

### Match Modes

| Mode | Description |
|------|-------------|
| `Exact` | Key must match exactly |
| `GreaterEqual` | First key >= search value |
| `Greater` | First key > search value |
| `Partial` | Key starts with search value |

### Field Types

| Type | DBL Equivalent | .NET Type |
|------|----------------|-----------|
| `Alpha` | `,a` | `string` |
| `Decimal` | `,d` | `long` or `decimal` |
| `Integer` | `,i` | `sbyte`, `short`, `int`, `long` |
| `PackedDecimal` | `,p` | `decimal` |
| `Date` | - | `DateOnly` |
| `Time` | - | `TimeOnly` |
| `DateTime` | - | `DateTime` |
| `Boolean` | - | `bool` |
| `Binary` | - | `byte[]` |

## Building Type-Safe Record Accessors

For strongly-typed access, extend `RecordAccessor<T>`:

```csharp
public class CustomerRecord
{
    public string CustomerId { get; set; } = "";
    public string Name { get; set; } = "";
    public decimal Balance { get; set; }
}

public class CustomerAccessor : RecordAccessor<CustomerRecord>
{
    private readonly FieldInfo _custId;
    private readonly FieldInfo _name;
    private readonly FieldInfo _balance;

    public CustomerAccessor(TableInfo table) : base(table)
    {
        _custId = table.GetField("cust_id")!;
        _name = table.GetField("name")!;
        _balance = table.GetField("balance")!;
    }

    public string CustomerId
    {
        get => GetAlpha(_custId);
        set => SetAlpha(_custId, value);
    }

    public string Name
    {
        get => GetAlpha(_name);
        set => SetAlpha(_name, value);
    }

    public decimal Balance
    {
        get => GetDecimalWithPlaces(_balance);
        set => SetDecimalWithPlaces(_balance, value);
    }

    public override CustomerRecord ToRecord() => new()
    {
        CustomerId = CustomerId,
        Name = Name,
        Balance = Balance
    };

    public override void FromRecord(CustomerRecord record)
    {
        Clear();
        CustomerId = record.CustomerId;
        Name = record.Name;
        Balance = record.Balance;
    }
}

// Usage
var accessor = new CustomerAccessor(file.Schema!.Tables[0]);
accessor.Load(file.Read("CUST0001"));
Console.WriteLine($"{accessor.CustomerId}: {accessor.Name} - ${accessor.Balance}");
```

## Multi-Table Files

Ziggy ISAM supports multiple record types in a single file, distinguished by a tag byte:

```csharp
using var file = IsamFile.Open("orders");
var schema = file.Schema!;

// Read a record
var data = file.Read("ORD00001");

// Determine table by tag byte
byte tag = data[schema.TagPosition];
var table = schema.GetTableByTag(tag);

Console.WriteLine($"Record type: {table?.Name}");
```

## Error Handling

Operations throw `ZiggyException` on failure:

```csharp
try
{
    var data = file.Read("NOTFOUND", MatchMode.Exact);
}
catch (ZiggyException ex) when (ex.ErrorCode == ZiggyError.KeyNotFound)
{
    Console.WriteLine("Record not found");
}
catch (ZiggyException ex) when (ex.ErrorCode == ZiggyError.EndOfFile)
{
    Console.WriteLine("End of file reached");
}
```

## ULIDs vs Traditional Keys

Ziggy ISAM uses ULIDs (Universally Unique Lexicographically Sortable Identifiers) as record identifiers:

| Feature | Traditional Key | ULID |
|---------|-----------------|------|
| Format | User-defined | 26-char Crockford Base32 |
| Uniqueness | Per-key | Global |
| Sortability | Key-dependent | Time-ordered |
| Generation | Manual | Automatic |
| Direct access | Via key index | Via ULID index |

```csharp
// Store and get ULID
string ulid = file.StoreWithUlid(record);
Console.WriteLine($"Stored with ULID: {ulid}");
// Output: Stored with ULID: 01ARZ3NDEKTSV4RRFFQ69G5FAV

// Later: direct access by ULID
var data = file.ReadByUlid(ulid);
```

## Platform Support

| Platform | Library Name | Status |
|----------|--------------|--------|
| macOS (arm64) | `libziggy_isam.dylib` | Tested |
| macOS (x64) | `libziggy_isam.dylib` | Should work |
| Linux (x64) | `libziggy_isam.so` | Should work |
| Windows (x64) | `ziggy_isam.dll` | Should work |

## Files in This Directory

| File | Description |
|------|-------------|
| `ZiggyIsam.cs` | .NET P/Invoke wrapper and managed classes |
| `ZiggyIsam.csproj` | .NET project file |
| `Example.cs` | Usage examples |
| `ziggy_isam.h` | C header for other language bindings |
| `README.md` | This documentation |

## See Also

- [ISAM Reference](../docs/isam-reference.md) - Full ISAM documentation
- [C ABI Documentation](../docs/cabi-reference.md) - Native function reference
