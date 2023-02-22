using System;
using System.Threading;

namespace CSharp
{
    public static class Keys
    {
        // Source: https://docs.microsoft.com/en-us/dotnet/api/system.console.keyavailable?view=netcore-3.1
        public static void Start()
        {
            ConsoleKeyInfo cki;

            do
            {
                WriteLine("\nPress a key to display; press the 'x' key to quit.");

                // Your code could perform some useful task in the following loop. However,
                // for the sake of this example we'll merely pause for a quarter second.

                while (Console.KeyAvailable == false)
                    Thread.Sleep(250); // Loop until input is entered.

                cki = Console.ReadKey(true);
                WriteLine("You pressed the '{0}' key.", cki.Key);
            } while (cki.Key != ConsoleKey.X);
        }
    }
}
