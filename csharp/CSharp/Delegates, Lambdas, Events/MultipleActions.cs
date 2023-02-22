using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;
using System.Threading.Tasks;

namespace CSharp
{
    public static class MultipleActions
    {
        public static void Start()
        {
            var o = new Outputter();
            o.OnAlarmRaised += () => WriteLine("hi");
            o.OnAlarmRaised += () => WriteLine("hey");
            o.Run();
        }

        public class Outputter
        {
            public Action? OnAlarmRaised { get; set; }

            public void Run()
            {
                OnAlarmRaised?.Invoke();
            }
        }
    }
}