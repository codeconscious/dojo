using System;
using System.Collections.Generic;
using System.Linq;
using System.Globalization;

namespace CSharp
{
    public static class DateTimeParseExact
    {
        public static void Start()
        {
            var array = new []
            {
                "11月13日",
                "1月13日",
                "1月3日",
                "13月13日",
                "11月32日",
                "11月31日",
                "2020年11月1日",
            };

            foreach (var text in array)
            {
                if (DateTime.TryParseExact(
                    text,
                    "yyyy年M月d日", //"M月d日",
                    CultureInfo.InvariantCulture,
                    DateTimeStyles.AssumeLocal,
                    out var date))
                {
                    WriteLine($"✔︎ Parsed {text} to {date}");
                }
                else
                {
                    WriteLine("× Could not parse " + text);
                }
            }
        }
    }
}
