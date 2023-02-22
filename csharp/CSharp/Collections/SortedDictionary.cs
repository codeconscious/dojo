using System;
using System.Linq;
using System.Collections;
using System.Collections.Generic;

// TODO: Add to the main menu
namespace CSharp
{
    public static class SortedDictionary
    {
        public static void Start()
        {
            var sd = new SortedDictionary<uint, string>();
            sd.Add(100, "Charles");
            sd.Add(1, "Scott");
            sd.Add(2, "Hank");

            foreach (var pair in sd)
                WriteLine($"#{pair.Key}: {pair.Value}");
        }
    }
}