using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text.RegularExpressions;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using static System.Console;

namespace CSharp
{
    public static class WhenAny
    {
        private static readonly Random _random = new();

        public async static Task Start()
        {
            try
            {
                //WriteLine("Started");
                var result5 = GetSquared(5);
                var result10 = GetSquared(100);
                var result23 = GetSquared(23);
                WriteLine("Back in main method");

                var tasks = new List<Task<int>> { result5, result10, result23 };

                while (tasks.Any())
                {
                    //WriteLine("Checking tasks...");
                    var finishedTask = await Task.WhenAny(tasks);

                    WriteLine(finishedTask.Result);

                    //WriteLine("Removing task...");
                    tasks.Remove(finishedTask);
                }
            }
            catch (Exception ex)
            {
                WriteLine("ERROR: " + ex.Message);
            }
        }

        static Task<int> GetSquared(short number)
        {
            WriteLine("  In submethod");
            var rnd = _random.Next(10);

            return Task.Run(() =>
            {
                Task.Delay(rnd * 1000);
                WriteLine("  Within Task.Run()...");
                return number * number;
            });
        }
    }
}
