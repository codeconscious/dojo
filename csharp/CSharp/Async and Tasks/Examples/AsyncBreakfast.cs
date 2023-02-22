using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace CSharp
{
    // Reference: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/

    class Coffee { }
    class Egg { }
    class Bacon { }
    class Toast { }
    class Juice { }

    static class AsyncBreakfast
    {
        static async Task Start(string[] args)
        {
            Coffee cup = PourCoffee();
            WriteLine("coffee is ready");

            var eggsTask = FryEggsAsync(2);
            var baconTask = FryBaconAsync(3);
            var toastTask = MakeToastWithButterAndJamAsync(2);

            var breakfastTasks = new List<Task> { eggsTask, baconTask, toastTask };
            while (breakfastTasks.Count > 0)
            {
                Task finishedTask = await Task.WhenAny(breakfastTasks);
                if (finishedTask == eggsTask)
                {
                    WriteLine("eggs are ready");
                }
                else if (finishedTask == baconTask)
                {
                    WriteLine("bacon is ready");
                }
                else if (finishedTask == toastTask)
                {
                    WriteLine("toast is ready");
                }
                breakfastTasks.Remove(finishedTask);
            }

            Juice oj = PourOJ();
            WriteLine("oj is ready");
            WriteLine("Breakfast is ready!");
        }

        static async Task<Toast> MakeToastWithButterAndJamAsync(int number)
        {
            var toast = await ToastBreadAsync(number);
            ApplyButter(toast);
            ApplyJam(toast);

            return toast;
        }

        private static Juice PourOJ()
        {
            WriteLine("Pouring orange juice");
            return new Juice();
        }

        private static void ApplyJam(Toast toast) =>
            WriteLine("Putting jam on the toast");

        private static void ApplyButter(Toast toast) =>
            WriteLine("Putting butter on the toast");

        private static async Task<Toast> ToastBreadAsync(int slices)
        {
            for (int slice = 0; slice < slices; slice++)
            {
                WriteLine("Putting a slice of bread in the toaster");
            }
            WriteLine("Start toasting...");
            await Task.Delay(3000);
            WriteLine("Remove toast from toaster");

            return new Toast();
        }

        private static async Task<Bacon> FryBaconAsync(int slices)
        {
            WriteLine($"putting {slices} slices of bacon in the pan");
            WriteLine("cooking first side of bacon...");
            await Task.Delay(3000);
            for (int slice = 0; slice < slices; slice++)
            {
                WriteLine("flipping a slice of bacon");
            }
            WriteLine("cooking the second side of bacon...");
            await Task.Delay(3000);
            WriteLine("Put bacon on plate");

            return new Bacon();
        }

        private static async Task<Egg> FryEggsAsync(int howMany)
        {
            WriteLine("Warming the egg pan...");
            await Task.Delay(3000);
            WriteLine($"cracking {howMany} eggs");
            WriteLine("cooking the eggs ...");
            await Task.Delay(3000);
            WriteLine("Put eggs on plate");

            return new Egg();
        }

        private static Coffee PourCoffee()
        {
            WriteLine("Pouring coffee");
            return new Coffee();
        }
    }
}