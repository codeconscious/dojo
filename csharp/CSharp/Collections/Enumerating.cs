using System;
using System.Collections.Generic;

namespace CSharp
{
    public static class Enumerating
    {
        public static void Start()
        {
            foreach (var number in CreateNumberCollection(3, 28, 2))
                Write(number + " ");
            WriteLine();
        }

        private static IEnumerable<int> CreateNumberCollection(int start, int end, byte step = 1)
        {
            var current = start;

            while (current < end)
            {
                yield return current;
                current += step;
            }
        }
    }
}
