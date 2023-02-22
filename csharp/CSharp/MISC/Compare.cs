using System;
using System.Linq;
using System.Collections.Generic;
using System.Text;

namespace CSharp
{
    public static class Comparison
    {
        public static void Compare()
        {
            var list = new List<uint> { 38345, 199, 32, 500, 729448, 490, 300, 2, 2044 };

            var sorted = new SortedSet<uint>(list);
            Write("Sorted: " + string.Join(", ", sorted));

            var comparison = SortMap[SortType.Normal];
            list.Sort(comparison);
            Write("Plain: " + string.Join(", ", list));

            Func<uint, uint> triple = (num) => num * 3;
            Write("Tripled: " + string.Join(", ", list.Select(triple)));
        }

        private enum SortType { Normal, Reverse }

        private static Dictionary<SortType, Comparison<uint>> SortMap =>
            new Dictionary<SortType, Comparison<uint>>
            {
                { SortType.Normal, new Comparison<uint>((x, y) => x.CompareTo(y)) },
                { SortType.Reverse, new Comparison<uint>((x, y) => y.CompareTo(x)) }
            };

        private static void Write(string text)
        {
            WriteLine(text);
        }
    }
}