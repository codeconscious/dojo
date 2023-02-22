namespace CSharp.Challenges;

// https://blog.bitsrc.io/most-important-javascript-coding-challenge-aa14c956d2df
public static class TenCommonJavaScriptQuestions
{
    public static void Start()
    {
        QuestionOne();
        QuestionTwo();
        QuestionThree();
        QuestionFour();
        QuestionNine();
    }

    /// <summary>
    /// Find the frequency of elements in array
    /// </summary>
    private static void QuestionOne()
    {
        var words = new List<string>() { "hello", "world", "c#", "hello", "c#" };

        //var counts = words.ToDictionary(w => w, w => w.Length);
        var counts = new Dictionary<string, int>();
        foreach (var word in words)
        {
            if (counts.ContainsKey(word))
                counts[word]++;
            else
                counts[word] = 1;
        }

        counts.ForEach(c => WriteLine(c.Key + " == " + c.Value));
    }

    /// <summary>
    /// Group items on the basis of age of given array of object
    /// </summary>
    private static void QuestionTwo()
    {
        var people = new List<(string Name, int Age)>()
        {
            new ("Bill", 43),
            new ("Lonni", 27),
            new ("Toff", 32),
            new ("Griff", 43),
            new ("Minami", 14),
            new ("Puu", 27),
        };

        var groups = people.GroupBy(p => p.Age);

        foreach (var group in groups)
        {
            WriteLine(group.Key);
            foreach (var (name, _) in group)
                WriteLine("  " + name);
        }
    }

    /// <summary>
    /// Find the longest word in a sentence
    /// </summary>
    private static void QuestionThree()
    {
        const string input = "It's time for our daily exercise";
        var longestWord = input
            .Split(" ")
            .OrderByDescending(w => w.Length)
            .First();
        WriteLine(longestWord);
    }

    /// <summary>
    /// Find the pairs of array element for which sum is equal to given target value (Two Sum Problem)
    /// </summary>
    private static void QuestionFour()
    {
        var array = new List<int>() { 1, 2, 3, 4, 6, 7, 8, 9 };
        const int target = 9;

        // Manual version
        //var pairs = new List<(int, int)>();
        //for (var i = 0; i < array.Count-1; i++)
        //{
        //    for (var j = 0; j < array.Count-1; j++)
        //    {
        //        if (i == j)
        //            continue;

        //        if (array[i] + array[j] == target)
        //            pairs.Add((array[i], array[j]));
        //    }
        //}

        // Method syntax
        var pairs = array.SelectMany(n1 => array,
                                     (n1, n2) => new ValueTuple<int, int>(n1, n2))
            .Where(pair => pair.Item1 <= Math.Ceiling(9 / 2f) &&
                           pair.Item1 + pair.Item2 == target);

        // Query syntax
        //var pairs = from n1 in array
        //            from n2 in array
        //            where n1 + n2 == target
        //            select new ValueTuple<int, int>(n1, n2);

        WriteLine(string.Join("; ", pairs.Select(p => p.Item1 + "+" + p.Item2)));
    }

    /// <summary>
    /// Print all duplicate elements of an array
    /// </summary>
    private static void QuestionNine()
    {
        var numbers = new[] { 1, 1, 5, 5, 7, 7, 8, 9, 3, 4, 4 };
        var existing = new List<int>();

        // Manual version
        //var duplicates = new HashSet<int>();
        //foreach (var number in numbers)
        //{
        //    if (existing.Contains(number))
        //        duplicates.Add(number);
        //    else
        //        existing.Add(number);
        //}

        // LINQ version -- might be much slower
        var duplicates = numbers.Where(n => numbers.Where(n2 => n2 == n).Count() > 1)
                                .Distinct();

        WriteLine("Duplicates: " + string.Join(", ", duplicates.OrderBy(n => n)));
    }
}


