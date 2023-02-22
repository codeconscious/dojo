using System;
using System.Linq;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Threading;

namespace CSharp
{
    public static class TaskParallelPractice
    {
        public static void MyTasks()
        {
            var myTasks = new List<Task>()
            {
                Task.Factory.StartNew(() => ImportantWork(1, 2000)),
                Task.Factory.StartNew(() => ImportantWork(2, 1000)),
                Task.Factory.StartNew(() => ImportantWork(3, 500)),
                Task.Factory.StartNew(() => ImportantWork(4, 500)),
            };

            //myTasks.

            var t1 = Task.Factory.StartNew(() => ImportantWork(1, 2000));
            var t2 = Task.Factory.StartNew(() => ImportantWork(2, 1000));
            var t3 = Task.Factory.StartNew(() => ImportantWork(3, 500));
            var t4 = Task.Factory.StartNew(() => ImportantWork(4, 500));

            WriteLine("Done?");
        }

        static void ImportantWork(int id, int duration)
        {
            WriteLine($"Task {id} begun.");
            Thread.Sleep(duration);
            WriteLine($"Task {id} complete.");
        }

        public static void Looping()
        {
            var intlist = new List<int> { 545,45345,246,6,2,34,632,634,2,643,62,642,63,634632,4 };

            Parallel.ForEach(intlist, (i) => Write(i + " "));
            WriteLine();

            Parallel.For(0, 50, (i) => Write(i + " "));
        }
    }
}