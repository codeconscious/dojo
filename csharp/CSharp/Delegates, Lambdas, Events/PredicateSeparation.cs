using System;
using System.Linq;
using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;
using System.Threading.Tasks;

namespace CSharp
{
    public static class PredicateSeparation
    {
        public static void Start()
        {
            var list = new List<string> {
                "dog", "cat", "bird", "reptile", "fish", "amphibian", "bug"
                }.AsReadOnly();

		    Func<string, bool> containsI = l => l.Contains("i"); // Predicate version
            var containsI_List = list.Where(containsI);
		    Print<string>(containsI_List);

            static bool containsA(string l) => l.Contains("a"); // Local function version
            var containsA_List = list.Where(containsA);
		    Print<string>(containsA_List);

            IReadOnlyCollection<byte> numbers =
                new HashSet<byte> { 0, 3, 4, 6, 94, 39, 30, 103, 255 };
            var isDivisibleBy6 = new Func<byte, bool>(n => n % 6 == 0);
            var numbersDivisibleBy6 = numbers.Where(isDivisibleBy6);
            Print<byte>(numbersDivisibleBy6);
        }

        public static void Print<T>(IEnumerable<T> list)
        {
            var output = "- " + string.Join(", ", list);
            WriteLine(output);
        }
    }
}