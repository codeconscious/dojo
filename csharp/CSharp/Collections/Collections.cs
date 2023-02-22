using System.Linq;
using System;
using System.Collections;
using System.Collections.Generic;

namespace CSharp
{
    public static class Collections
    {
        public static void Start()
        {
            var a = new List<object>
            {
                "a",
                144,
                new string[3] { "a", "b", "c" }
            };

            foreach (var item in a)
                WriteLine(item.GetType());
        }
    }
}