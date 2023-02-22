namespace CSharp
{
    /// <summary>
    /// Aggregate is like reduce() in JS. See https://stackoverflow.com/q/7105505/11767771.
    /// </summary>
    public static class Aggregate
    {
        public static void Start()
        {
            IReadOnlyList<int> numbers = new List<int> { 0,1,2,3,4,5,6,7,8,9,10 };

            // Even numbers
            var even = numbers.Aggregate(
                (total, current) =>
                    current % 2 == 0
                        ? total + 1
                        : total);
            WriteLine("Even count: " + string.Join(", ", even));

            // Addition
            var sumWithSeed = numbers.Aggregate(
                10, // seed -- i.e., the initial value
                (total, current) => total + current);
            WriteLine("Sum (seeded): " + sumWithSeed);
            var sumNoSeed = numbers.Aggregate(
                (total, next) => total + next);
            WriteLine("Sum (unseeded): " + sumNoSeed);

            // Multiplication (seeded and not)
            var productWithSeed = numbers
                .Where(n => n != 0)
                .Aggregate(5, (running, current) => running * current);
            WriteLine("Multiple (seeded): " + productWithSeed.ToString("#,##0"));
            var productNoSeed = numbers
                .Where(n => n != 0)
                .Aggregate((running, num) => running * num);
            WriteLine("Multiple (seedless): " + productNoSeed.ToString("#,##0"));

            // Reverse string
            const string input = "This is a really cool method";
            var inputs = input.Split(' ');
            var reversed = inputs.Aggregate(
                (running, current) => current + " | " + running);
            WriteLine(reversed);

            // ROT13 using a StringBuilder
            const string originalText = "Uryyb jbeyq."; //"Hello world.";
            var rot13 = originalText.Aggregate(
                new StringBuilder(),
                (sb, current) =>
                {
                    if (!char.IsLetter(current))
                        return sb.Append(current);

                    var working = (char) (current + 13);

                    // The IsUpper() calls are needed to ensure that
                    // uppercase letters don't bleed into lowercase ones.
                    return char.IsLetter(working) && char.IsUpper(current) == char.IsUpper(working)
                        ? sb.Append(working)
                        : sb.Append((char)(current - 13));
                });
            WriteLine("ROT13: " + rot13);
        }
    }
}