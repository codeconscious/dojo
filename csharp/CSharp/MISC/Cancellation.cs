using System;
using System.Linq;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Threading.Tasks;


namespace CSharp
{
    public static class Cancellation
    {
        static CancellationTokenSource cancellationTokenSource = new CancellationTokenSource();
        //static bool shouldCancel = false;

        static void Clock()
        {
            while (!cancellationTokenSource.IsCancellationRequested)
            //while (!shouldCancel)
            {
                Thread.Sleep(500);
                WriteLine("Tick...");
            }
        }

        public static void Start()
        {
            Task.Run(() => Clock());
            WriteLine("Press any key to finish...");
            Console.ReadKey();

            cancellationTokenSource.Cancel();
            //shouldCancel = true;

            WriteLine("Stopped!");
            Console.ReadKey();
        }
    }
}