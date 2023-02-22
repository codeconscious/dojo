using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;
using System.Threading.Tasks;

namespace CSharp
{
    public static class ConsoleColors
    {
        /// <summary>
        /// Show all available colors on the console.
        /// </summary>
        /// <see cref="https://docs.microsoft.com/en-us/dotnet/api/system.console.resetcolor?view=net-5.0"/>
        public static void Start()
        {
            // Get an array with the values of ConsoleColor enumeration members.
            ConsoleColor[] colors = (ConsoleColor[])ConsoleColor.GetValues(typeof(ConsoleColor));
            // Save the current background and foreground colors.
            ConsoleColor currentBackground = Console.BackgroundColor;
            ConsoleColor currentForeground = Console.ForegroundColor;

            // Display all foreground colors except the one that matches the background.
            WriteLine("All the foreground colors except {0}, the background color:",
                              currentBackground);
            foreach (var color in colors)
            {
                if (color == currentBackground) continue;

                Console.ForegroundColor = color;
                WriteLine("   The foreground color is {0}.", color);
            }
            WriteLine();
            // Restore the foreground color.
            Console.ForegroundColor = currentForeground;

            // Display each background color except the one that matches the current foreground color.
            WriteLine("All the background colors except {0}, the foreground color:",
                              currentForeground);
            foreach (var color in colors)
            {
                if (color == currentForeground) continue;

                Console.BackgroundColor = color;
                WriteLine("   The background color is {0}.", color);
            }

            // Restore the original console colors.
            Console.ResetColor();
            WriteLine("\nOriginal colors restored...");
        }
    }
}