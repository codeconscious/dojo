namespace CSharp
{
    public static class Lookups
    {
        public static void Start()
        {
            IList<string> strings = new List<string>
            {
                "az", "be", "as", "ce", "da", "ab", "bg"
            };

            ILookup<char, string> stringsByFirstChar = strings.ToLookup(s => s[0]);

            foreach (var lookup in stringsByFirstChar)
            {
                WriteLine(lookup.Key);
                foreach (var item in lookup.OrderBy(l => l))
                {
                    WriteLine("   " + item);
                }
            }
        }

        /// <summary>
        /// Prints the numbers in a groups of 3.
        /// </summary>
        /// <see cref="https://mydevtricks.com/linq-gems-tolookup"/>
        public static void StartOneToMany()
        {
            var numbers = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 };

            ILookup<int, int> groupedNumbersLookup =
                numbers
                    .Select((num, seq) => (num, seq))
                    .ToLookup(key => (key.seq / 3), val => val.num);

            var i = 0;
            while (groupedNumbersLookup.Contains(i))
            {
                WriteLine($"GROUP {i + 1}: " +
                          string.Join(',', groupedNumbersLookup[i]));
                i++;
            }
        }
    }
}