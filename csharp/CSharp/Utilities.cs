namespace CSharp
{
    public static class Utilities
    {
        public static string GetStringOfApprovedChars(byte length)
        {
            Random rnd = new();

            const string allowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

            int allowedCharsLength = allowedChars.Length;

            var sb = new StringBuilder();

            while (length > 0)
            {
                var thisRnd = rnd.Next(allowedCharsLength);
                sb.Append(allowedChars[thisRnd]);
                length--;
            }

            return sb.ToString();
        }

        public static void PrintRainbowText(string text)
        {
            ConsoleColor[] colors = new[]
            {
                ConsoleColor.Blue,
                ConsoleColor.Cyan,
                ConsoleColor.Green,
                ConsoleColor.Magenta,
                ConsoleColor.Yellow,
                ConsoleColor.Red,
                ConsoleColor.Gray,
            };

            ConsoleColor? previousColor = null;

            foreach (var letter in text)
            {
                var thisColor = GetConsoleColorExcluding(colors, previousColor);

                Console.ForegroundColor = thisColor;
                Write(letter);
                previousColor = thisColor;
            }

            Console.ResetColor();
            WriteLine();

            static ConsoleColor GetConsoleColorExcluding(ConsoleColor[] colors, ConsoleColor? unwantedColor)
            {
                Random _rnd = new();

                ConsoleColor color;
                do
                {
                    color = colors[_rnd.Next(colors.Length)];
                }
                while (unwantedColor == color);

                return color;
            }
        }
    }
}
