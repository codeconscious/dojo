using System.Linq;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Dynamic;

namespace CSharp
{
    public static class Dynamic
    {
        public static void Start()
        {
            var lists = new List<dynamic>
            {
                new List<dynamic> { 1, 3, 5, 7, 9 },
                new List<dynamic> { 1.3M, 3, 5, 7, 9 },
                // "string text",
                new List<dynamic> { "hello", "world" },
            };

            foreach (var list in lists)
            {
                WriteLine("ITEM: " + list);
                WriteLine("Contains: " + list.Count + " items");
                WriteLine("Type: " + list.GetType());

                var pattern = "";
                if (list != null)
                {
                    for (var i = 0; i < list.Count; i++)
                        pattern += "{" + i + "}, ";
                }
                else
                {
                    pattern = "{0}";
                }

                WriteLine($"  Pattern: {pattern}");
                WriteLine($"  Argument count: {list?.Count ?? 0}");
                WriteLine("  OUTPUT: " + string.Format(pattern, list?.ToArray() ?? "[None!]"));
                WriteLine();
            }
        }
    }
}
