using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using static System.Console;

namespace CSharp
{
    public static class Immutable
    {
        public static void Start()
        {
            // From an empty collection
            var original = ImmutableList.Create<int>(0, 1);
            var version1 = original.Add(2);
            var version2 = version1.Add(3);
            var version3 = version2.AddRange(new List<int> { 4, 5, 6 });
            var lastItem = version3.Last();
            var version4 = version3.Remove(lastItem);
            WriteLine(string.Join(", ", version4));

            // Using a builder (Available to some collection types)
            var builder = ImmutableList.CreateBuilder<int>();
            builder.Add(0);
            builder.Add(1);
            builder.AddRange(new List<int> { 2, 3, 4 });
            var firstItem = builder.First();
            builder.Remove(firstItem);
            WriteLine(string.Join(", ", builder.ToImmutable()));
        }
    }
}