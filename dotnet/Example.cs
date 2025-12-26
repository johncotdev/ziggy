// Example.cs - Demonstrates using Ziggy ISAM from .NET
//
// This example shows how to:
// 1. Create and open ISAM files
// 2. Store and read records
// 3. Use ULIDs for direct record access
// 4. Iterate through records sequentially
// 5. Access schema information

using System;
using System.Text;
using Ziggy.Isam;

namespace ZiggyExample
{
    // Example: Typed record accessor for a customer record
    //
    // Record layout:
    //   Position 0-7:   cust_id (alpha, 8 bytes)
    //   Position 8-37:  name (alpha, 30 bytes)
    //   Position 38-47: balance (decimal, 10 bytes, 2 decimal places)
    //
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
            _custId = table.GetField("cust_id") ?? throw new Exception("Missing cust_id field");
            _name = table.GetField("name") ?? throw new Exception("Missing name field");
            _balance = table.GetField("balance") ?? throw new Exception("Missing balance field");
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

        public override CustomerRecord ToRecord()
        {
            return new CustomerRecord
            {
                CustomerId = CustomerId,
                Name = Name,
                Balance = Balance
            };
        }

        public override void FromRecord(CustomerRecord record)
        {
            Clear();
            CustomerId = record.CustomerId;
            Name = record.Name;
            Balance = record.Balance;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine($"=== {ZiggyInfo.Name} v{ZiggyInfo.Version} ===\n");

            // Example 1: Basic ISAM operations with byte arrays
            BasicExample();

            // Example 2: Using ULIDs for direct record access
            UlidExample();

            // Example 3: Sequential iteration
            SequentialExample();

            Console.WriteLine("\nAll examples completed successfully!");
        }

        static void BasicExample()
        {
            Console.WriteLine("--- Basic ISAM Operations ---\n");

            const int RecordSize = 48;
            const int KeyStart = 0;
            const int KeyLength = 8;

            // Create a new ISAM file
            using (var file = IsamFile.Create("/tmp/dotnet_example", RecordSize, KeyStart, KeyLength))
            {
                // Store some records
                var records = new[]
                {
                    "CUST0001Acme Corporation         0000015000",
                    "CUST0002Beta Industries          0000008500",
                    "CUST0003Gamma Systems            0000022000"
                };

                foreach (var rec in records)
                {
                    byte[] data = Encoding.ASCII.GetBytes(rec.PadRight(RecordSize));
                    file.Store(data);
                    Console.WriteLine($"Stored: {rec.Substring(0, 8)}");
                }

                file.Flush();
            }

            // Reopen and read
            using (var file = IsamFile.Open("/tmp/dotnet_example"))
            {
                Console.WriteLine("\nReading by key 'CUST0002':");
                var record = file.Read("CUST0002");
                Console.WriteLine($"  Found: {Encoding.ASCII.GetString(record)}");
            }

            Console.WriteLine();
        }

        static void UlidExample()
        {
            Console.WriteLine("--- ULID Record Access ---\n");

            const int RecordSize = 64;
            string? savedUlid = null;

            // Create file and store with ULID
            using (var file = IsamFile.Create("/tmp/dotnet_ulid", RecordSize, 0, 8))
            {
                byte[] record = Encoding.ASCII.GetBytes("PROD0001Widget Pro           2999".PadRight(RecordSize));
                savedUlid = file.StoreWithUlid(record);
                Console.WriteLine($"Stored record with ULID: {savedUlid}");
            }

            // Later: Access directly by ULID
            using (var file = IsamFile.Open("/tmp/dotnet_ulid"))
            {
                Console.WriteLine($"\nReading by ULID: {savedUlid}");
                var record = file.ReadByUlid(savedUlid!);
                Console.WriteLine($"  Found: {Encoding.ASCII.GetString(record)}");
            }

            Console.WriteLine();
        }

        static void SequentialExample()
        {
            Console.WriteLine("--- Sequential Iteration ---\n");

            const int RecordSize = 32;

            // Create file with multiple records
            using (var file = IsamFile.Create("/tmp/dotnet_seq", RecordSize, 0, 4))
            {
                for (int i = 1; i <= 5; i++)
                {
                    string data = $"K{i:D3}Record number {i}".PadRight(RecordSize);
                    file.Store(Encoding.ASCII.GetBytes(data));
                }
            }

            // Sequential read
            using (var file = IsamFile.Open("/tmp/dotnet_seq"))
            {
                Console.WriteLine("Reading all records in key order:");

                // Position at first record
                try
                {
                    file.Read("K001", MatchMode.GreaterEqual);

                    int count = 0;
                    while (count < 10) // Safety limit
                    {
                        var ulid = file.GetCurrentUlid();
                        if (count > 0)
                        {
                            try
                            {
                                var record = file.ReadNext();
                                Console.WriteLine($"  {Encoding.ASCII.GetString(record).TrimEnd()} (ULID: {ulid?.Substring(0, 10)}...)");
                            }
                            catch (ZiggyException ex) when (ex.ErrorCode == ZiggyError.EndOfFile)
                            {
                                break;
                            }
                        }
                        else
                        {
                            // First record already read by Read()
                            Console.WriteLine($"  First record positioned (ULID: {ulid?.Substring(0, 10)}...)");
                        }
                        count++;
                    }
                }
                catch (ZiggyException ex)
                {
                    Console.WriteLine($"Error: {ex.ErrorCode}");
                }
            }

            Console.WriteLine();
        }
    }
}
