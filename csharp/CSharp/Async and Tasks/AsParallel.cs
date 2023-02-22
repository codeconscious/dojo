using System.Linq;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Dynamic;
using System.Diagnostics;

namespace CSharp
{
    public static class AsParallel
    {
        public static void Start()
        {
            var things = new List<Thing>();

            var stopwatch = new Stopwatch();
            stopwatch.Start();
            foreach (uint num in Enumerable.Range(1, 100000000))
                things.Add(new Thing(num));
            WriteLine($"Enumeration took {stopwatch.ElapsedMilliseconds:#,000}ms.");

            stopwatch.Restart();
            foreach (var thing in things.Where(t => t.Squared % Math.Pow(5000000,2) == 0))
                Write(thing.Number + " ");
            WriteLine($"\nNormal operation took {stopwatch.ElapsedMilliseconds:#,000}ms.");

            stopwatch.Restart();
            foreach (var thing in things.AsParallel().Where(t => t.Squared % Math.Pow(5000000,2) == 0))
                Write(thing.Number + " ");
            WriteLine($"\nParallel operation took {stopwatch.ElapsedMilliseconds:#,000}ms.");
        }

        private class Thing
        {
            public uint Number { get; set; }
            public double Squared => Math.Pow(Number, 2);

            public Thing(uint id)
            {
                Number = id;
            }
        }
    }
}