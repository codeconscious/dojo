using System.Linq;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Dynamic;
using System.Diagnostics;

namespace CSharp
{
    public static class DateTimeParse
    {
        public static void Start()
        {
            var input = new Dictionary<string, string>
            {
                { "September 3", "d MMMM" },
                { "３月９日", "d MMMM" },
                { "3月9日", "d MMMM" },
                { "２００２年４月１４日", "MMM dd, yyyy" },
                { "2002年4月14日", "MMM dd, yyyy" },
            };

            foreach (var i in input)
            {
                if (DateTime.TryParse(i.Key, out var date))
                    WriteLine(date.ToString(i.Value));
                else
                    WriteLine($"ERROR: Could not parse \"{i.Key}\".");
            }
        }
    }
}
