using System;
using System.Linq;
using System.Diagnostics.CodeAnalysis;

namespace CSharp.VersionTesting
{
	/// <summary>
	/// Learning about what's new in C# 11.
    /// Link: https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-11
	/// </summary>
    public static class Version11
	{
		public static void Start()
		{
            WriteLine("""
                This is a long message.
                It has several lines.
                    Some are indented
                            more than others.
                Some should start at the first column.
                Some have "quoted text" in them.
                """);

            // ReadOnlySpan<byte> utf8 = "This is a UTF-8 ReadOnlySpan"u8;

            var cyclops = new Person("Scott", "Summers");
            var iceman = new Person
            {
                FirstName = "Robert", // Both names are "required" to be set.
                LastName = "Drake"
            };
            WriteLine(cyclops);
            WriteLine(iceman);

            WriteLine(Add(4, 5.2m));
            WriteLine(Sum(new double[] {2, 4, 6, 8.3}));
            WriteLine(Multiply(new nuint[] {2, 4, 6, 8}));
            WriteLine(Multiply(CreateNumberArray(10)).ToString("#,##0"));
        }

        static nuint[] CreateNumberArray(uint count)
        {
            var array = new nuint[count];
            var rnd = new Random();
            for (var i = 0; i < count; i++)
            {
                array[i] = (nuint) rnd.Next(1, 100);
                Write(array[i] + ", ");
            }
            WriteLine();
            return array;
        }

        static T Add<T>(T left, T right) where T : INumber<T>
        {
            return left + right;
        }

        static T Sum<T>(params T[] numbers) where T : INumber<T>
        {
            T total = T.Zero;
            foreach (var number in numbers)
                total += number;
            return total;
        }

        static T Multiply<T>(params T[] numbers) where T : IUnsignedNumber<T>
        {
            T total = T.One;
            foreach (var number in numbers)
                total *= number;
            return total;
        }
	}

    file class Person
    {
        public Person() { }

        public required string FirstName { get; init; }
        public required string LastName { get; init; }

        [SetsRequiredMembers]
        public Person(string firstName, string lastName)
        {
            FirstName = firstName;
            LastName = lastName;
        }

        public override string ToString() => FirstName + " " + LastName;
    }
}
