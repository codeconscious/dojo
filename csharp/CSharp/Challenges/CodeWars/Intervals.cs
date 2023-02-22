using System;
using System.Linq;

namespace CSharp
{
    // https://www.codewars.com/kata/52b7ed099cdc285c300001cd/train/csharp
    public static class Intervals
    {
        public static void Start()
        {
            WriteLine(SumIntervals());
        }

        private static int SumIntervals()
        {
            var intervals = new (int, int)[] { (-1, 4), (-5, -3) };

            return intervals
                .SelectMany(i => Enumerable.Range(i.Item1, i.Item2 - i.Item1))
                .Distinct()
                .Count();
        }
    }
}