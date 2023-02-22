using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Immutable;
using static System.Console;

namespace CSharp
{
    public static class TranslationMemory
    {
        public static void Start()
        {
            IDistanceCalculator distanceCalculator = new DamerauLevenshteinDistanceCalculator();

            var segments = new List<SegmentText>
            {
                new ("This is a perfectly fine sentence.",
                     "This, too, is a pretty nice sentence, I'd say. Don't even get me started on this one."),
                new ("これはたぶん日本語ですね。",
                     "まさか、これも日本語かよ？"),
                new ("そんなことでいいか？",
                     "そんなことでよいか？"),
                new ("It was a sunny day in the neighborhood when she appeared. I'd never seen her before, but there was clearly something unusual about her.",
                     "This is a completely different sentence. Even its length is different."),
                new ("It was a sunny day in the neighborhood when she appeared. I'd never seen her before, but there was clearly something unusual about her.",
                     "This is a completely different sentence. Even its length is different. It was a sunny day in the neighborhood when she appeared."),
                new ("BIG", "BOG"),
            };

            foreach (var segment in segments)
            {
                var distance = distanceCalculator.CalculateDistance(segment);
                var length = Math.Max(segment.Source.Length, segment.Target.Length);

                ResultPrinter(segment, distance, length);
            }
        }

        private static void ResultPrinter(IHasSegmentText text, int distance, int length)
        {
            WriteLine("Source: " + text.Source);
            WriteLine("Target: " + text.Target);
            WriteLine("Distance: " + distance);
            WriteLine("Score: " + Math.Max(100 - distance, 0));
            WriteLine();
        }
    }

    #region Segments
    public interface IHasSegmentText
    {
        string Source { get; }
        string Target { get; }
    }

    public record SegmentText(string Source, string Target) : IHasSegmentText;
    #endregion

    #region Calculator
    public interface IDistanceCalculator
    {
        int CalculateDistance(string source, string target);
        int CalculateDistance(IHasSegmentText text);
    }

    public class DamerauLevenshteinDistanceCalculator : IDistanceCalculator
    {
        /// <summary>
        /// Gets the Damerau-Levenshtein distance for two strings.
        /// </summary>
        /// <param name="textContainer">An object containing both source and target text.</param>
        /// <reference>https://www.csharpstar.com/csharp-string-distance-algorithm/</reference>
        public int CalculateDistance(IHasSegmentText textContainer)
            => CalculateDistance(textContainer.Source, textContainer.Target);

        /// <summary>
        /// Gets the Damerau-Levenshtein distance for two strings.
        /// </summary>
        /// <param name="source">Source document text.</param>
        /// <param name="target">Target translation text.</param>
        /// <reference>https://www.csharpstar.com/csharp-string-distance-algorithm/</reference>
        public int CalculateDistance(string source, string target)
        {
            var bounds = new { Height = source.Length + 1, Width = target.Length + 1 };

            int[,] matrix = new int[bounds.Height, bounds.Width];

            for (int height = 0; height < bounds.Height; height++) { matrix[height, 0] = height; };
            for (int width = 0; width < bounds.Width; width++) { matrix[0, width] = width; };

            for (int height = 1; height < bounds.Height; height++)
            {
                for (int width = 1; width < bounds.Width; width++)
                {
                    int cost = (source[height - 1] == target[width - 1]) ? 0 : 1;
                    int insertion = matrix[height, width - 1] + 1;
                    int deletion = matrix[height - 1, width] + 1;
                    int substitution = matrix[height - 1, width - 1] + cost;

                    int distance = Math.Min(insertion, Math.Min(deletion, substitution));

                    if (height > 1 && width > 1 && source[height - 1] == target[width - 2] && source[height - 2] == target[width - 1])
                    {
                        distance = Math.Min(distance, matrix[height - 2, width - 2] + cost);
                    }

                    matrix[height, width] = distance;
                }
            }

            return matrix[bounds.Height - 1, bounds.Width - 1];
        }
    }
    #endregion
}