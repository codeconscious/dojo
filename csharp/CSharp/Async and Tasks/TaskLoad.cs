using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace CSharp
{
    public static class TaskLoad
    {
        public static void Start()
        {
            const int iterations = 10_000_000;
            const int groupAtEvery = iterations / 2;
            var tasks = new Task[50];

            for (var taskNo = 0; taskNo < tasks.Length; taskNo++)
            {
                var thisTaskNo = taskNo;
                tasks[thisTaskNo] = new Task(() =>
                {
                    for (var i = 1; i <= iterations; i++)
                    {
                        var thisIterationNo = i;
                        if (thisIterationNo % groupAtEvery == 0)
                            Write($"{thisTaskNo}:{thisIterationNo}  ");
                    }
                });
            }

            Parallel.ForEach(tasks, task => task.Start());
            Task.WaitAll(tasks);
            WriteLine();
        }
    }
}