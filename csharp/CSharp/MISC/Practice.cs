namespace CSharp
{
    public static class Practice
    {
        /// <summary>
        /// Removes duplicate characters when the same character is repeated multiple times.
        /// </summary>
        public static void RemoveConsecutiveDuplicates()
        {
            const string text = "Heeey, have you seen a moose in Haarlem?? Noooo!!!";

            var output = RemoveConsecutiveDuplicates(text);
            var outputLinq = RemoveConsecutiveDuplicatesLinq(text);
            WriteLine($"Output Nrml: {output}");
            WriteLine($"Output Linq: {outputLinq}");

            static string RemoveConsecutiveDuplicates(string text)
            {
                StringBuilder sb = new(text.Length);
                sb.Append(text[0]);

                char previousChar = default;

                for (var i = 1; i < text.Length; i++)
                {
                    var currentChar = text[i];
                    if (currentChar != previousChar)
                    {
                        sb.Append(currentChar);
                        previousChar = currentChar;
                    }
                }

                return sb.ToString();
            }

            static string RemoveConsecutiveDuplicatesLinq(string text)
            {
                var chars = text?.Select((value, index) => new { value, index })?
                                 .Where(pair => pair.index == 0 ||
                                                pair.value != text[pair.index - 1])?
                                 .Select(l => l.value)
                            ?? Enumerable.Empty<char>();

                return string.Concat(chars);
            }
        }

        public static void ReverseText()
        {
            const string text = "What in the world are you doing?";
            var reversed = ReverseTextManually(text);
            WriteLine(reversed);

            static string ReverseTextManually(string text)
            {
                StringBuilder sb = new(text.Length);

                for (int i = text.Length - 1; i >= 0; i--)
                    sb.Append(text[i]);

                return sb.ToString();
            }
        }
    }
}