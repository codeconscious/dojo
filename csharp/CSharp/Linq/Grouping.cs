namespace CSharp
{
    public static class Grouping
    {
        public static void Start()
        {
            IReadOnlyList<int> numbers = new List<int> { 0,1,1,2,3,4,4,5,5,5,6,7,8,9 };

            var groupedNumbersWithDupes =
                from number in numbers
                group number by number into grouped
                where grouped.Count() > 1
                select grouped;

            WriteLine(string.Join(", ", groupedNumbersWithDupes.ToList().Select(i => i.Key.ToString())));
        }
    }
}