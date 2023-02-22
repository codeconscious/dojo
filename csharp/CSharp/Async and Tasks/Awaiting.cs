using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

/*
    Note that await/async doesn't natively work with console applications!
    https://stackoverflow.com/questions/28840188/why-is-asynccontext-needed-when-using-async-await-with-a-console-application
    https://blog.stephencleary.com/2012/02/async-console-programs.html
    https://stackoverflow.com/questions/54935233/my-console-app-shutdown-prematurely-when-using-async-await
*/

namespace CSharp
{
    public static class AwaitPractice
    {
        public static void Start()
        {
            WriteLine($"[Main] Preparing to call the async caller method...");
            //StandBy(8).Wait(); // works
            //StandByTwice(3).Wait();
            CallStandByAsync().Wait(); // This must be a blocking wait.
            WriteLine($"[Main] All async method calls done!");
        }

        public static Task CallStandByAsync()
        {
            var tasks = new Task[3];
            WriteLine($"  [Caller] Starting async calls...");
            tasks[0] = StandByAsync(6);
            WriteLine($"  [Caller] First async call started.");
            tasks[1] = StandByAsync(4);
            WriteLine($"  [Caller] Second async call started.");
            tasks[2] = StandByAsync(2);
            WriteLine($"  [Caller] Third async call started.");
            return Task.WhenAll(tasks); //.ContinueWith(t1 => WriteLine(t1));
        }

        public static async Task StandByAsync(int a)
        {
            WriteLine($"    [Async] Starting to sleep for {a} seconds...");
            // var task = Task.Delay(a * 1000);
            // await task;
            await Task.Delay(a * 1000);
            WriteLine($"    [Async] {a}-second sleep done.");
        }

        // public static async Task StandBy(int a)
        // {
        //     WriteLine($"  [Async] Starting to sleep for {a} seconds...");
        //     //await Task.Delay(a * 1000); // Doesn't work; see https://stackoverflow.com/a/14071859/11767771
        //     var wait1 = Task.Delay(a * 1000);
        //     await wait1;
        //     WriteLine($"  [Async] Starting to sleep for another {a} seconds...");
        //     var wait2 = Task.Delay(a * 1000);
        //     await wait2;
        //     WriteLine($"  [Async] Slept twice.");
        // }

        // public static async Task StandByTwice(int a)
        // {
        //     WriteLine($"  [Async] Starting to sleep for {a} and {a * 2} seconds...");
        //     var wait1 = Task.Delay(a * 1000);
        //     var wait2 = Task.Delay(a * 1000 * 2);
        //     WriteLine($"  [Async] Requests made...");
        //     await wait1;
        //     await wait2;
        //     WriteLine($"  [Async] Slept twice.");
        // }
    }
}