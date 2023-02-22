using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace CSharp
{
    public static class Threads
    {
        public static void Start()
        {
            // Better to just use tasks:
            // https://stackoverflow.com/questions/4190949/create-multiple-threads-and-wait-all-of-them-to-complete

            var t1 = new Thread(() => Write("hi "));

            const int iterations = 1000000;
            const int step = iterations / 4;

            var t2 = new Thread(() =>
            {
                for (var x = 0; x <= iterations; x++)
                    if (x % step == 0)
                        Write(Thread.CurrentThread.Name + x + " ");
            });
            t2.Name = "T2:";

            var t3 = new Thread(() =>
            {
                for (var x = 0; x <= iterations; x++)
                    if (x % step == 0)
                        Write(Thread.CurrentThread.Name + x + " ");
            })
            {
                Name = "T3:"
            };

            t1.Start();
            t2.Start();
            t3.Start();

            t1.Join();
            t2.Join();
            t3.Join();

            WriteLine();
        }
    }
}