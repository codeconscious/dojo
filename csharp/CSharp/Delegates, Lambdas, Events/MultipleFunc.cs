using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;
using System.Threading.Tasks;

namespace CSharp
{
    public static class MultipleFuncs
    {
        public static void Start()
        {
            // https://stackoverflow.com/questions/29999098/calling-delegate-with-multiple-functions-having-return-values

            var o = new Outputter();
            const string text = "Super Luigi";
            o.OnAlarmRaised += (string t) => t.ToUpperInvariant(); // Ignored?
            o.OnAlarmRaised += (string t) => new string(t.Reverse().ToArray());
            WriteLine(o.Run(text));
        }

        public class Outputter
        {
            public Func<string, string>? OnAlarmRaised { get; set; }

            public string Run(string t)
            {
                return OnAlarmRaised?.Invoke(t) ?? "";
            }
        }
    }
}
