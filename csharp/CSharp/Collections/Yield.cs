namespace CSharp
{
    public static class Yield
    {
        public static void Start()
        {
            foreach (var number in GetXNumbersEnumerable(5))
            {
                Write(number + "  ");
            }
            WriteLine();

            var enumerator = GetXNumbersEnumerator(5);
            while (enumerator.MoveNext())
            {
                 Write(enumerator.Current + "  ");
            }
            WriteLine();
        }

        private static IEnumerable<int> GetXNumbersEnumerable(int numberOfNumbers)
        {
            var current = 0;

            do
            {
                yield return current++;
            } while (current < numberOfNumbers);
        }

        // See https://stackoverflow.com/a/61228891/11767771.
        private static IEnumerator<int> GetXNumbersEnumerator(int numberOfNumbers)
        {
            var current = 0;

            while (current < numberOfNumbers)
            {
                 yield return current++;
            }
        }
    }
}