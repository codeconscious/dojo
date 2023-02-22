using System;
using System.Threading;
using System.Threading.Tasks;

namespace CSharp
{
    // Reference: https://www.reddit.com/r/csharp/comments/mh8bnq/expert_level_c_concepts/gsyjye6?utm_source=share&utm_medium=web2x&context=3
    public static class ProperlyDoneAsync
    {
        static AsyncLocal<string> _asyncLocalString = new AsyncLocal<string>();

        static ThreadLocal<string> _threadLocalString = new ThreadLocal<string>();

        public static async Task Start()
        {
            WriteLine("Top method #1 in");
            await WhenAButtonIsClicked(true);
            WriteLine("Top method #1 out");

            WriteLine();
            WriteLine("Top method #2 in");
            await WhenAButtonIsClicked(false);
            WriteLine("Top method #2 out");

            Thread.Sleep(10000);
            WriteLine("Done");
        }

        private static async Task WhenAButtonIsClicked(bool useBadVersion)
        {
            WriteLine("  UI method in"); // 1
            if (useBadVersion)
                await BadMethodAsync();  // 2
            else
                await BetterMethodAsync();
            WriteLine("  UI method out"); // 3
        }

        // Bad version:
        private static async Task BadMethodAsync()
        {
            WriteLine("    Bad submethod in"); // 4
            Thread.Sleep(5000); // 5
            await Task.CompletedTask; // 6
            WriteLine("    Bad submethod out"); // 7
        }

        // Less bad version:
        private static Task BetterMethodAsync()
        {
            WriteLine("    Less-bad submethod in"); // 4
            return Task.Run(() =>
            {
                WriteLine("      Less-bad submethod task in");
                Thread.Sleep(5000); // 6  // PROBLEM: Program stops here.
                WriteLine("      Less-bad submethod task out");
            }); // 5
                // 7
        }
    }
}