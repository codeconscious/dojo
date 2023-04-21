using System;
using System.Runtime.CompilerServices;
using System;
using System.Collections.Immutable;
using System.Linq;
using LanguageExt;
using static LanguageExt.Prelude;
using static System.Console;

namespace CSharp.NuGetPackages;

public static class FunctionalCSharp
{
	public static void Start()
	{
        static int EvalWith5ThenAdd2(Func<int, int> fn) => fn(5) + 2;
        var square = (int x) => x * x;
        WriteWithExpression(EvalWith5ThenAdd2(square));

        static Func<int, int> MultiplierGenerator(int toMultiply) => x => x * toMultiply;
        var multiplyBy3 = MultiplierGenerator(3);
        WriteWithExpression(multiplyBy3(2));
        var multiplyBy200 = MultiplierGenerator(200);
        WriteWithExpression(multiplyBy200(2));
        var multiplyBy100000 = MultiplierGenerator(multiplyBy200(500));
        WriteWithExpression(multiplyBy100000(7));

        // Generate a list of random multipliers, then write the output of each having passed in the same int to each.
        var rnd = new Random();
        var number = rnd.Next(15);
        var randomMultipliers = Enumerable.Range(1, 5)
            .Select(_ => MultiplierGenerator(rnd.Next(1000))) // Maybe try .NextBytes?
            .ToImmutableList();
        randomMultipliers.ForEach(multiplier => WriteWithExpression(multiplier(number)));

        /// <summary>
        /// Writes an expression and its output.
        /// </summary>
        /// <remarks>The CallerArgumentExpression attribute is great!</remarks>
        /// <param name="result"></param>
        /// <param name="expression"></param>
        static void WriteWithExpression(object result, [CallerArgumentExpression("result")] string? expression = null)
        {
            if (!string.IsNullOrWhiteSpace(expression))
            {
                ForegroundColor = ConsoleColor.Cyan;
                WriteLine("＞" + expression);
                ResetColor();
            }

            var output = result switch
            {
                int i => i.ToString("#,##0"),
                double d => d.ToString("#,##0.#"),
                decimal d => d.ToString("#,##0.#"),
                _ => result?.ToString() ?? "<NULL>"
            };

            WriteLine("   " + output);
        }
    }
}
