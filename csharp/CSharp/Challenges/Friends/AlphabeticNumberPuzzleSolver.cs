namespace CSharp.Challenges;

public static class AlphabeticNumberPuzzleSolver
{
    public static void Run()
    {
        Calculate(ArithmeticOperationType.Multiplication, "ABCDE", "A", "EEEEEE");
        Calculate(ArithmeticOperationType.Addition, "SEND", "MORE", "MONEY");
    }

    /// <summary>
    /// Calculates the answer to the given problem.
    /// </summary>
    /// <param name="desiredOperation">A desired arithmetic calcuation type.</param>
    /// <param name="rawWords">A word is comprised of all letters on a line.</param>
    private static void Calculate(ArithmeticOperationType desiredOperation,
                                  params string[] rawWords)
    {
        if (rawWords.Length != 3)
        {
            WriteLine("Three words must be provided. You provided " +
                $"{rawWords.Length}: {string.Join(", ", rawWords)}.");
            return;
        }

        var stopwatch = new System.Diagnostics.Stopwatch();
        stopwatch.Start();

        IReadOnlyList<string> words = new List<string>(rawWords.Select(t => t.Trim()));

        IReadOnlyList<char> numberSet = "0123456789".ToList();

        WriteLine(Environment.NewLine + $"Words: {string.Join(", ", rawWords)}");

        var distinctLetters = words.SelectMany(str => str.ToCharArray()).Distinct();

        var distinctLetterCount = distinctLetters.Distinct().Count();
        if (distinctLetterCount > numberSet.Count)
        {
            WriteLine("Failure: Not enough numbers for the count of distinct letters.");
            return;
        }

        var operation = ArithmeticOperations.Create(desiredOperation);

        WriteLine("Distinct chars: " + string.Concat(distinctLetters));
        WriteLine("Operation: " + operation.Name);
        WriteLine("Possible numbers: " + string.Join(", ", numberSet));

        // Map the locations of each distinct letter in the word array
        // so they can be replaced with numbers later.
        var map = distinctLetters.ToDictionary(c => c, _ => new List<(int X, int Y)>());
        foreach (var distinctLetter in distinctLetters)
        {
            // This avoids two additional for loops -- lovely.r
            map[distinctLetter].AddRange(
                words.SelectMany((word, wordIndex) =>
                    word.Select((_, letterIndex) => (wordIndex, letterIndex))));
        }

        var numberPermutations = GetPermutations(numberSet, distinctLetterCount);

        // TODO: Make this whole thing more efficient.
        foreach (var numberCollection in numberPermutations)
        {
            var number = numberCollection.ToList();

            var charsWithValues = new Dictionary<char, int>(distinctLetterCount);

            for (var i = 0; i < map.Keys.Count; i++)
            {
                charsWithValues.Add(
                    map.Keys.ToArray()[i],
                    int.Parse(number[i].ToString()));
            }

            var newStrings = new List<string>(words).Select(t => new StringBuilder(t))
                                                    .ToList();

            for (var wordIndex = 0; wordIndex < newStrings.Count; wordIndex++)
            {
                for (var letterIndex = 0; letterIndex < newStrings[wordIndex].Length; letterIndex++)
                {
                    var thisChar = newStrings[wordIndex][letterIndex];
                    var numericValue = charsWithValues[thisChar];
                    newStrings[wordIndex][letterIndex] = numericValue.ToString()[0];
                }
            }

            var strings = newStrings.Select(t => t.ToString()).ToList();

            // If any number string starts with 0, it's eliminated.
            // TODO: Make this more dynamic.
            if (strings.Any(s => s.StartsWith('0')))
                continue;

            // Convert the strings of numbers to actual numbers.
            var testNumbers = strings.Select(t => int.Parse(t)).ToList();

            // Check the test case and stop if we pass.
            if (operation.Execute(testNumbers[0], testNumbers[1], testNumbers[2]))
            {
                WriteLine($"SOLUTION: {testNumbers[0]} {operation.Symbol} {testNumbers[1]} == {testNumbers[2]}");
                WriteLine("Done in " + stopwatch.Elapsed.TotalSeconds.ToString("#,##0.00s"));
                return;
            }
        }

        WriteLine("Failed after " + stopwatch.Elapsed.TotalSeconds.ToString("#,##0.00s"))   ;
    }

    private enum ArithmeticOperationType : byte
    {
        Addition,
        Subtraction,
        Multiplication,
        Division
    }

    private class ArithmeticOperations
    {
        public static IArithmeticOperation Create(ArithmeticOperationType type)
        {
            ArgumentNullException.ThrowIfNull(type);

            return type switch
            {
                ArithmeticOperationType.Addition => new Addition(),
                ArithmeticOperationType.Subtraction => new Subtraction(),
                ArithmeticOperationType.Multiplication => new Multiplication(),
                ArithmeticOperationType.Division => new Division(),
                _ => throw new InvalidOperationException("Invalid arithmetic operation.")
            };
        }

        public interface IArithmeticOperation
        {
            /// <summary>
            /// A delegate containing the requested operation for solution checking.
            /// </summary>
            Func<int, int, int, bool> Execute { get; }

            /// <summary>
            /// The symbol of the operation (e.g., '+' or '/').
            /// </summary>
            char Symbol { get; }

            string Name { get; }
        }

        private class Addition : IArithmeticOperation
        {
            public Func<int, int, int, bool> Execute =>
                (addend1, addend2, expectedSum) =>
                    addend1 + addend2 == expectedSum;

            public char Symbol => '+';

            public string Name => nameof(Addition);
        }

        private class Subtraction : IArithmeticOperation
        {
            public Func<int, int, int, bool> Execute =>
                (minuend, subtrahend, expectedDifference) =>
                    minuend - subtrahend == expectedDifference;

            public char Symbol => '-';

            public string Name => nameof(Subtraction);
        }

        private class Multiplication : IArithmeticOperation
        {
            public Func<int, int, int, bool> Execute =>
                (factor1, factor2, expectedProduct) =>
                    factor1 * factor2 == expectedProduct;

            public char Symbol => '*';

            public string Name => nameof(Multiplication);
        }

        private class Division : IArithmeticOperation
        {
            public Func<int, int, int, bool> Execute =>
                (dividend, divisor, expectedQuotient) =>
                    dividend * divisor == expectedQuotient;

            public char Symbol => '/';

            public string Name => nameof(Division);
        }
    }

    // Source: https://stackoverflow.com/a/10630026/11767771
    private static IEnumerable<IEnumerable<T>> GetPermutations<T>(IEnumerable<T> list, int length)
    {
        if (length == 1)
            return list.Select(t => new T[] { t });

        return GetPermutations(list, length - 1)
            .SelectMany(t =>
                list.Where(e => !t.Contains(e)),
                (t1, t2) => t1.Concat(new T[] { t2 }));
    }
}
