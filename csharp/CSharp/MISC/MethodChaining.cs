using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;
using System.Threading.Tasks;

namespace CSharp
{
    public static class MethodChaining
    {
        public static void Start()
        {
            var thing = new Thing().Add(5).Subtract(4).Add(30).Add(10);
            WriteLine(thing.Number);
        }

        private class Thing
        {
            public uint Number;

            public Thing Add(uint num)
            {
                this.Number += num;
                return this;
            }

            public Thing Subtract(uint num)
            {
                this.Number -= num;
                return this;
            }
        }
    }
}