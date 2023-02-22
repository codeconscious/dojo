using System;
using System.Linq;
using System.Collections.Generic;

namespace CSharp
{
    public static class Joins
    {
        // https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/join-clause
        public static void Start()
        {
            var ints = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30 };

            var people = new Dictionary<int, string>
            {
                { 15, "Mario" },
                { 10, "Luigi" },
                { 4, "Peach" },
            };

            // Inner join
            var output = from i in ints
                         join person in people on i equals person.Key
                         select i;

            WriteLine(string.Join(", ", output));

            // Group join
            var output2 = from i in ints
                          join person in people on i equals person.Key into grouped
                          select new { Number = i, People = grouped.DefaultIfEmpty() };

            foreach (var group in output2)
                WriteLine(group.Number + ": " + string.Join(", ", group.People));
        }
    }
}
