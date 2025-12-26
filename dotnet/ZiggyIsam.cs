// Ziggy ISAM .NET Interop
//
// This file provides P/Invoke wrappers for calling the Ziggy ISAM native library
// from .NET applications. It includes:
// - Native function declarations
// - Safe wrapper classes
// - Schema introspection
// - Record accessor base class

using System;
using System.Runtime.InteropServices;
using System.Text;

namespace Ziggy.Isam
{
    /// <summary>
    /// Error codes returned by Ziggy ISAM operations
    /// </summary>
    public enum ZiggyError
    {
        Ok = 0,
        FileNotFound = -1,
        InvalidFormat = -2,
        CorruptedIndex = -3,
        KeyNotFound = -4,
        DuplicateKey = -5,
        RecordLocked = -6,
        EndOfFile = -7,
        InvalidKey = -8,
        OutOfMemory = -9,
        IoError = -10,
        InvalidUlid = -11,
        UlidNotFound = -12,
        InvalidHandle = -13,
        InvalidArgument = -14
    }

    /// <summary>
    /// Field data types (matching DBL types)
    /// </summary>
    public enum FieldType : byte
    {
        Alpha = 0,          // ,a - Character string (space-padded)
        Decimal = 1,        // ,d - Implied decimal (ASCII digits)
        Integer = 2,        // ,i - Binary integer (signed)
        PackedDecimal = 3,  // ,p - Packed decimal (BCD)
        Date = 10,          // Date stored as YYYYMMDD decimal
        Time = 11,          // Time stored as HHMMSS decimal
        DateTime = 12,      // Combined date/time
        Boolean = 13,       // Single byte 0/1
        Binary = 14         // Raw bytes (no conversion)
    }

    /// <summary>
    /// Match modes for READ operations
    /// </summary>
    public enum MatchMode
    {
        Exact = 0,
        GreaterEqual = 1,
        Greater = 2,
        Partial = 3
    }

    /// <summary>
    /// Open modes for ISAM files
    /// </summary>
    public enum OpenMode
    {
        ReadOnly = 0,
        ReadWrite = 1,
        Exclusive = 2
    }

    /// <summary>
    /// Native P/Invoke declarations for libziggy_isam
    /// </summary>
    internal static class NativeMethods
    {
        private const string LibraryName = "ziggy_isam";

        // File Operations
        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ziggy_create(
            [MarshalAs(UnmanagedType.LPStr)] string filename,
            uint recordSize,
            uint keyStart,
            uint keyLength);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ziggy_create_with_schema(
            [MarshalAs(UnmanagedType.LPStr)] string filename,
            uint recordSize,
            uint keyStart,
            uint keyLength,
            IntPtr schemaHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ziggy_open(
            [MarshalAs(UnmanagedType.LPStr)] string filename,
            int mode);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ziggy_close(IntPtr handle);

        // Record Operations
        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern ZiggyError ziggy_store(
            IntPtr handle,
            byte[] record,
            uint recordLen);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern ZiggyError ziggy_store_with_ulid(
            IntPtr handle,
            byte[] record,
            uint recordLen,
            byte[] ulidOut);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern ZiggyError ziggy_read(
            IntPtr handle,
            byte[] key,
            uint keyLen,
            byte[] recordOut,
            uint recordSize,
            out uint bytesRead);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern ZiggyError ziggy_read_with_mode(
            IntPtr handle,
            byte[] key,
            uint keyLen,
            int matchMode,
            byte[] recordOut,
            uint recordSize,
            out uint bytesRead);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern ZiggyError ziggy_read_next(
            IntPtr handle,
            byte[] recordOut,
            uint recordSize,
            out uint bytesRead);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern ZiggyError ziggy_read_by_ulid(
            IntPtr handle,
            byte[] ulidStr,
            byte[] recordOut,
            uint recordSize,
            out uint bytesRead);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern ZiggyError ziggy_write(
            IntPtr handle,
            byte[] record,
            uint recordLen);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern ZiggyError ziggy_delete(IntPtr handle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern void ziggy_unlock(IntPtr handle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern ZiggyError ziggy_flush(IntPtr handle);

        // ULID Access
        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ziggy_get_current_ulid(IntPtr handle, byte[] ulidOut);

        // Schema Operations
        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ziggy_get_schema(IntPtr handle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ziggy_schema_is_multi_table(IntPtr schemaHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_schema_table_count(IntPtr schemaHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_schema_tag_position(IntPtr schemaHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_schema_description_len(IntPtr schemaHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_schema_description(IntPtr schemaHandle, byte[] buf, uint bufLen);

        // Table Operations
        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ziggy_schema_get_table(IntPtr schemaHandle, uint index);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ziggy_schema_get_table_by_tag(IntPtr schemaHandle, byte tag);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_table_name_len(IntPtr tableHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_table_name(IntPtr tableHandle, byte[] buf, uint bufLen);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern byte ziggy_table_tag(IntPtr tableHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_table_record_size(IntPtr tableHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_table_field_count(IntPtr tableHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern int ziggy_table_is_default(IntPtr tableHandle);

        // Field Operations
        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ziggy_table_get_field(IntPtr tableHandle, uint index);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_field_name_len(IntPtr fieldHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_field_name(IntPtr fieldHandle, byte[] buf, uint bufLen);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern byte ziggy_field_type(IntPtr fieldHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_field_position(IntPtr fieldHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern uint ziggy_field_length(IntPtr fieldHandle);

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern byte ziggy_field_decimal_places(IntPtr fieldHandle);

        // Version Info
        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ziggy_version();

        [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr ziggy_name();
    }

    /// <summary>
    /// Exception thrown by Ziggy ISAM operations
    /// </summary>
    public class ZiggyException : Exception
    {
        public ZiggyError ErrorCode { get; }

        public ZiggyException(ZiggyError error)
            : base($"Ziggy ISAM error: {error}")
        {
            ErrorCode = error;
        }

        public ZiggyException(ZiggyError error, string message)
            : base(message)
        {
            ErrorCode = error;
        }
    }

    /// <summary>
    /// Represents a field definition in a table schema
    /// </summary>
    public class FieldInfo
    {
        public string Name { get; }
        public FieldType Type { get; }
        public uint Position { get; }
        public uint Length { get; }
        public byte DecimalPlaces { get; }

        internal FieldInfo(IntPtr handle)
        {
            uint nameLen = NativeMethods.ziggy_field_name_len(handle);
            byte[] nameBuf = new byte[nameLen];
            NativeMethods.ziggy_field_name(handle, nameBuf, nameLen);
            Name = Encoding.UTF8.GetString(nameBuf);

            Type = (FieldType)NativeMethods.ziggy_field_type(handle);
            Position = NativeMethods.ziggy_field_position(handle);
            Length = NativeMethods.ziggy_field_length(handle);
            DecimalPlaces = NativeMethods.ziggy_field_decimal_places(handle);
        }

        /// <summary>
        /// Get the .NET type that corresponds to this field
        /// </summary>
        public Type DotNetType => Type switch
        {
            FieldType.Alpha => typeof(string),
            FieldType.Decimal when DecimalPlaces > 0 => typeof(decimal),
            FieldType.Decimal => typeof(long),
            FieldType.Integer when Length == 1 => typeof(sbyte),
            FieldType.Integer when Length == 2 => typeof(short),
            FieldType.Integer when Length == 4 => typeof(int),
            FieldType.Integer => typeof(long),
            FieldType.PackedDecimal => typeof(decimal),
            FieldType.Date => typeof(DateOnly),
            FieldType.Time => typeof(TimeOnly),
            FieldType.DateTime => typeof(DateTime),
            FieldType.Boolean => typeof(bool),
            FieldType.Binary => typeof(byte[]),
            _ => typeof(byte[])
        };
    }

    /// <summary>
    /// Represents a table definition in a schema
    /// </summary>
    public class TableInfo
    {
        public string Name { get; }
        public byte Tag { get; }
        public uint RecordSize { get; }
        public bool IsDefault { get; }
        public FieldInfo[] Fields { get; }

        internal TableInfo(IntPtr handle)
        {
            uint nameLen = NativeMethods.ziggy_table_name_len(handle);
            byte[] nameBuf = new byte[nameLen];
            NativeMethods.ziggy_table_name(handle, nameBuf, nameLen);
            Name = Encoding.UTF8.GetString(nameBuf);

            Tag = NativeMethods.ziggy_table_tag(handle);
            RecordSize = NativeMethods.ziggy_table_record_size(handle);
            IsDefault = NativeMethods.ziggy_table_is_default(handle) != 0;

            uint fieldCount = NativeMethods.ziggy_table_field_count(handle);
            Fields = new FieldInfo[fieldCount];
            for (uint i = 0; i < fieldCount; i++)
            {
                IntPtr fieldHandle = NativeMethods.ziggy_table_get_field(handle, i);
                if (fieldHandle != IntPtr.Zero)
                {
                    Fields[i] = new FieldInfo(fieldHandle);
                }
            }
        }

        /// <summary>
        /// Get a field by name
        /// </summary>
        public FieldInfo? GetField(string name)
        {
            foreach (var field in Fields)
            {
                if (field.Name == name) return field;
            }
            return null;
        }
    }

    /// <summary>
    /// Represents a database schema with table definitions
    /// </summary>
    public class SchemaInfo
    {
        public string Description { get; }
        public uint TagPosition { get; }
        public bool IsMultiTable { get; }
        public TableInfo[] Tables { get; }

        internal SchemaInfo(IntPtr handle)
        {
            uint descLen = NativeMethods.ziggy_schema_description_len(handle);
            if (descLen > 0)
            {
                byte[] descBuf = new byte[descLen];
                NativeMethods.ziggy_schema_description(handle, descBuf, descLen);
                Description = Encoding.UTF8.GetString(descBuf);
            }
            else
            {
                Description = "";
            }

            TagPosition = NativeMethods.ziggy_schema_tag_position(handle);
            IsMultiTable = NativeMethods.ziggy_schema_is_multi_table(handle) != 0;

            uint tableCount = NativeMethods.ziggy_schema_table_count(handle);
            Tables = new TableInfo[tableCount];
            for (uint i = 0; i < tableCount; i++)
            {
                IntPtr tableHandle = NativeMethods.ziggy_schema_get_table(handle, i);
                if (tableHandle != IntPtr.Zero)
                {
                    Tables[i] = new TableInfo(tableHandle);
                }
            }
        }

        /// <summary>
        /// Get a table by name
        /// </summary>
        public TableInfo? GetTable(string name)
        {
            foreach (var table in Tables)
            {
                if (table.Name == name) return table;
            }
            return null;
        }

        /// <summary>
        /// Get a table by tag byte
        /// </summary>
        public TableInfo? GetTableByTag(byte tag)
        {
            foreach (var table in Tables)
            {
                if (table.Tag == tag) return table;
            }
            return null;
        }
    }

    /// <summary>
    /// ISAM file handle with safe managed wrapper
    /// </summary>
    public class IsamFile : IDisposable
    {
        private IntPtr _handle;
        private bool _disposed;
        private readonly uint _recordSize;
        private byte[] _recordBuffer;
        private SchemaInfo? _schema;

        /// <summary>
        /// Gets the record size for this file
        /// </summary>
        public uint RecordSize => _recordSize;

        /// <summary>
        /// Gets the schema for this file (if available)
        /// </summary>
        public SchemaInfo? Schema
        {
            get
            {
                if (_schema == null)
                {
                    IntPtr schemaHandle = NativeMethods.ziggy_get_schema(_handle);
                    if (schemaHandle != IntPtr.Zero)
                    {
                        _schema = new SchemaInfo(schemaHandle);
                    }
                }
                return _schema;
            }
        }

        private IsamFile(IntPtr handle, uint recordSize)
        {
            _handle = handle;
            _recordSize = recordSize;
            _recordBuffer = new byte[recordSize];
        }

        /// <summary>
        /// Create a new ISAM file
        /// </summary>
        public static IsamFile Create(string filename, uint recordSize, uint keyStart, uint keyLength)
        {
            IntPtr handle = NativeMethods.ziggy_create(filename, recordSize, keyStart, keyLength);
            if (handle == IntPtr.Zero)
            {
                throw new ZiggyException(ZiggyError.IoError, "Failed to create ISAM file");
            }
            return new IsamFile(handle, recordSize);
        }

        /// <summary>
        /// Open an existing ISAM file
        /// </summary>
        public static IsamFile Open(string filename, OpenMode mode = OpenMode.ReadWrite)
        {
            IntPtr handle = NativeMethods.ziggy_open(filename, (int)mode);
            if (handle == IntPtr.Zero)
            {
                throw new ZiggyException(ZiggyError.FileNotFound, "Failed to open ISAM file");
            }
            // TODO: Get record size from schema or header
            // For now, use a default buffer size
            return new IsamFile(handle, 4096);
        }

        /// <summary>
        /// Store a new record
        /// </summary>
        public void Store(byte[] record)
        {
            var error = NativeMethods.ziggy_store(_handle, record, (uint)record.Length);
            if (error != ZiggyError.Ok)
            {
                throw new ZiggyException(error);
            }
        }

        /// <summary>
        /// Store a new record and return its ULID
        /// </summary>
        public string StoreWithUlid(byte[] record)
        {
            byte[] ulidBuf = new byte[26];
            var error = NativeMethods.ziggy_store_with_ulid(_handle, record, (uint)record.Length, ulidBuf);
            if (error != ZiggyError.Ok)
            {
                throw new ZiggyException(error);
            }
            return Encoding.ASCII.GetString(ulidBuf);
        }

        /// <summary>
        /// Read a record by key
        /// </summary>
        public ReadOnlySpan<byte> Read(byte[] key, MatchMode mode = MatchMode.Exact)
        {
            var error = NativeMethods.ziggy_read_with_mode(
                _handle, key, (uint)key.Length,
                (int)mode, _recordBuffer, (uint)_recordBuffer.Length,
                out uint bytesRead);

            if (error != ZiggyError.Ok)
            {
                throw new ZiggyException(error);
            }
            return _recordBuffer.AsSpan(0, (int)bytesRead);
        }

        /// <summary>
        /// Read a record by key string
        /// </summary>
        public ReadOnlySpan<byte> Read(string key, MatchMode mode = MatchMode.Exact)
        {
            return Read(Encoding.ASCII.GetBytes(key), mode);
        }

        /// <summary>
        /// Read the next sequential record
        /// </summary>
        public ReadOnlySpan<byte> ReadNext()
        {
            var error = NativeMethods.ziggy_read_next(
                _handle, _recordBuffer, (uint)_recordBuffer.Length,
                out uint bytesRead);

            if (error != ZiggyError.Ok)
            {
                throw new ZiggyException(error);
            }
            return _recordBuffer.AsSpan(0, (int)bytesRead);
        }

        /// <summary>
        /// Read a record by ULID
        /// </summary>
        public ReadOnlySpan<byte> ReadByUlid(string ulid)
        {
            byte[] ulidBytes = Encoding.ASCII.GetBytes(ulid);
            var error = NativeMethods.ziggy_read_by_ulid(
                _handle, ulidBytes, _recordBuffer, (uint)_recordBuffer.Length,
                out uint bytesRead);

            if (error != ZiggyError.Ok)
            {
                throw new ZiggyException(error);
            }
            return _recordBuffer.AsSpan(0, (int)bytesRead);
        }

        /// <summary>
        /// Update the current record
        /// </summary>
        public void Write(byte[] record)
        {
            var error = NativeMethods.ziggy_write(_handle, record, (uint)record.Length);
            if (error != ZiggyError.Ok)
            {
                throw new ZiggyException(error);
            }
        }

        /// <summary>
        /// Delete the current record
        /// </summary>
        public void Delete()
        {
            var error = NativeMethods.ziggy_delete(_handle);
            if (error != ZiggyError.Ok)
            {
                throw new ZiggyException(error);
            }
        }

        /// <summary>
        /// Unlock the current record
        /// </summary>
        public void Unlock()
        {
            NativeMethods.ziggy_unlock(_handle);
        }

        /// <summary>
        /// Flush buffers to disk
        /// </summary>
        public void Flush()
        {
            var error = NativeMethods.ziggy_flush(_handle);
            if (error != ZiggyError.Ok)
            {
                throw new ZiggyException(error);
            }
        }

        /// <summary>
        /// Get the ULID of the current record
        /// </summary>
        public string? GetCurrentUlid()
        {
            byte[] ulidBuf = new byte[26];
            if (NativeMethods.ziggy_get_current_ulid(_handle, ulidBuf) != 0)
            {
                return Encoding.ASCII.GetString(ulidBuf);
            }
            return null;
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (!_disposed)
            {
                if (_handle != IntPtr.Zero)
                {
                    NativeMethods.ziggy_close(_handle);
                    _handle = IntPtr.Zero;
                }
                _disposed = true;
            }
        }

        ~IsamFile()
        {
            Dispose(false);
        }
    }

    /// <summary>
    /// Base class for type-safe record accessors
    /// </summary>
    /// <typeparam name="T">The record type</typeparam>
    public abstract class RecordAccessor<T> where T : new()
    {
        protected readonly TableInfo _table;
        protected readonly byte[] _buffer;

        protected RecordAccessor(TableInfo table)
        {
            _table = table;
            _buffer = new byte[table.RecordSize];
        }

        /// <summary>
        /// Get the raw record buffer
        /// </summary>
        public Span<byte> Buffer => _buffer;

        /// <summary>
        /// Load record data from a span
        /// </summary>
        public void Load(ReadOnlySpan<byte> data)
        {
            data.CopyTo(_buffer);
        }

        /// <summary>
        /// Clear the buffer
        /// </summary>
        public void Clear()
        {
            Array.Fill(_buffer, (byte)' ');
        }

        /// <summary>
        /// Read an alpha field as string
        /// </summary>
        protected string GetAlpha(FieldInfo field)
        {
            return Encoding.ASCII.GetString(_buffer, (int)field.Position, (int)field.Length).TrimEnd();
        }

        /// <summary>
        /// Set an alpha field from string
        /// </summary>
        protected void SetAlpha(FieldInfo field, string value)
        {
            // Pad or truncate to field length
            byte[] bytes = Encoding.ASCII.GetBytes(value.PadRight((int)field.Length).Substring(0, (int)field.Length));
            Array.Copy(bytes, 0, _buffer, field.Position, field.Length);
        }

        /// <summary>
        /// Read a decimal field as long (no decimal places)
        /// </summary>
        protected long GetDecimal(FieldInfo field)
        {
            string str = Encoding.ASCII.GetString(_buffer, (int)field.Position, (int)field.Length).Trim();
            if (string.IsNullOrEmpty(str) || str.All(c => c == ' ' || c == '0'))
                return 0;
            return long.Parse(str);
        }

        /// <summary>
        /// Read a decimal field with implied decimals
        /// </summary>
        protected decimal GetDecimalWithPlaces(FieldInfo field)
        {
            long rawValue = GetDecimal(field);
            if (field.DecimalPlaces == 0) return rawValue;
            return rawValue / (decimal)Math.Pow(10, field.DecimalPlaces);
        }

        /// <summary>
        /// Set a decimal field
        /// </summary>
        protected void SetDecimal(FieldInfo field, long value)
        {
            string formatted = value.ToString().PadLeft((int)field.Length, '0');
            if (formatted.Length > field.Length)
                formatted = formatted.Substring(formatted.Length - (int)field.Length);
            byte[] bytes = Encoding.ASCII.GetBytes(formatted);
            Array.Copy(bytes, 0, _buffer, field.Position, field.Length);
        }

        /// <summary>
        /// Set a decimal field with implied decimals
        /// </summary>
        protected void SetDecimalWithPlaces(FieldInfo field, decimal value)
        {
            long rawValue = (long)(value * (decimal)Math.Pow(10, field.DecimalPlaces));
            SetDecimal(field, rawValue);
        }

        /// <summary>
        /// Read an integer field
        /// </summary>
        protected long GetInteger(FieldInfo field)
        {
            return field.Length switch
            {
                1 => (sbyte)_buffer[field.Position],
                2 => BitConverter.ToInt16(_buffer, (int)field.Position),
                4 => BitConverter.ToInt32(_buffer, (int)field.Position),
                8 => BitConverter.ToInt64(_buffer, (int)field.Position),
                _ => 0
            };
        }

        /// <summary>
        /// Set an integer field
        /// </summary>
        protected void SetInteger(FieldInfo field, long value)
        {
            byte[] bytes = field.Length switch
            {
                1 => new[] { (byte)(sbyte)value },
                2 => BitConverter.GetBytes((short)value),
                4 => BitConverter.GetBytes((int)value),
                8 => BitConverter.GetBytes(value),
                _ => new byte[field.Length]
            };
            Array.Copy(bytes, 0, _buffer, field.Position, Math.Min(bytes.Length, (int)field.Length));
        }

        /// <summary>
        /// Convert buffer to typed record
        /// </summary>
        public abstract T ToRecord();

        /// <summary>
        /// Load typed record into buffer
        /// </summary>
        public abstract void FromRecord(T record);
    }

    /// <summary>
    /// Version information for Ziggy ISAM
    /// </summary>
    public static class ZiggyInfo
    {
        public static string Version
        {
            get
            {
                IntPtr ptr = NativeMethods.ziggy_version();
                return Marshal.PtrToStringAnsi(ptr) ?? "unknown";
            }
        }

        public static string Name
        {
            get
            {
                IntPtr ptr = NativeMethods.ziggy_name();
                return Marshal.PtrToStringAnsi(ptr) ?? "Ziggy ISAM";
            }
        }
    }
}
