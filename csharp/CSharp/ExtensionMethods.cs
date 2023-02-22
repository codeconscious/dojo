using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace CSharp
{
    public static class ExtensionMethods
    {
        // https://stackoverflow.com/q/3569811/11767771
        public static bool IsNonStringEnumerable(this PropertyInfo pi) =>
            pi?.PropertyType.IsNonStringEnumerable() == true;

        public static bool IsNonStringEnumerable(this object instance) =>
            instance?.GetType().IsNonStringEnumerable() == true;

        public static bool IsNonStringEnumerable(this Type type) =>
            type != null &&
            type != typeof(string)
            && typeof(IEnumerable).IsAssignableFrom(type);

        public static bool IsLosslesslyCastableToInt(this decimal number) =>
            number <= int.MaxValue &&
            number >= int.MinValue &&
            number == Math.Truncate(number);

        public static bool IsLosslesslyCastableToUInt(this decimal number) =>
            number <= uint.MaxValue &&
            number >= uint.MinValue &&
            number == Math.Truncate(number);

        public static void ForEach<T>(this IEnumerable<T> source, Action<T> action)
        {
            foreach (var item in source)
                action(item);
        }

        /// <summary>
        /// A version of Contains allowing for various string comparisons (e.g., for case insensitivity).
        /// </summary>
        /// <seealso cref="https://stackoverflow.com/a/444818"/>
        /// <param name="source"></param>
        /// <param name="toCheck"></param>
        /// <param name="comparison"></param>
        /// <returns></returns>
        public static bool Contains(this string source, string toCheck, StringComparison comparison)
        {
            return source?.IndexOf(toCheck, comparison) >= 0;
        }

        /// <summary>
        /// Remove n items from the end of a collection.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="list"></param>
        /// <param name="startAt"></param>
        public static void RemoveRangeFromEnd<T>(this List<T> list, int startAt)
        {
            list.RemoveRange(list.Count - startAt, startAt);
        }

        /// <summary>
        /// Split collections into multiple equally-sized ones.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="collection"></param>
        /// <param name="parts">The number of parts (batches) to split the collection into.</param>
        /// <see cref="https://stackoverflow.com/a/5304033/11767771"/>
        /// <returns>A collection of sub-collections.</returns>
        public static IEnumerable<IEnumerable<T>> Split<T>(this IEnumerable<T> collection, ushort parts)
        {
            if (collection.Count() <= parts)
                return new List<IEnumerable<T>> { collection };

            return collection.Select((item, index) => new { index, item })
                             .GroupBy(x => x.index % parts)
                             .Select(x => x.Select(y => y.item));
        }

        /// <summary>
        /// Flatten a two-dimensional array into a single IEnumerable.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="map"></param>
        /// <see cref="https://stackoverflow.com/a/3150821/11767771"/>
        /// <returns></returns>
        public static IEnumerable<T> Flatten<T>(this T[,] map)
        {
            for (int row = 0; row < map.GetLength(0); row++)
            {
                for (int col = 0; col < map.GetLength(1); col++)
                {
                    yield return map[row, col];
                }
            }
        }

        /// <summary>
        /// Determines if any values in two range overlap.
        /// </summary>
        public static bool DoRangesOverlap(Tuple<int, int> testPair,
                                           ISet<Tuple<int, int>> testAgainstPairs)
        {
            if (testPair == null)
                throw new ArgumentNullException(nameof(testPair));

            if (testAgainstPairs== null)
                throw new ArgumentNullException(nameof(testAgainstPairs));

            var testRange = Enumerable.Range(testPair.Item1, testPair.Item2);
            //WriteLine(string.Join(", ", testRange));

            var testAgainstRange = testAgainstPairs.SelectMany(l => Enumerable.Range(l.Item1, l.Item2))
                                                   .Distinct();
            //WriteLine(string.Join(", ", testAgainstRange));

            return testRange.Intersect(testAgainstRange).Any();
        }

        /// <summary>
        /// Use ForEach on any IEnumerable.
        /// </summary>
        /// <param name="ts"></param>
        /// <param name="action"></param>
        /// <typeparam name="T"></typeparam>
        /// <source>"Functional Programming in C#" (book)</source>
        /// <returns></returns>
        //public static IEnumerable<Unit> ForEach<T>(this IEnumerable<T> ts, Action<T> action)
        //    => ts.Map(action.ToFunc()).ToImmutableList();
    }
}