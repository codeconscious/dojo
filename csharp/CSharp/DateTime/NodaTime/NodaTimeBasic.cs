using System;
using NodaTime;

namespace CSharp
{
    public static class NodaTimeBasic
    {
        public static void Start()
        {
            var localDate1 = new NodaTime.LocalDate(2013, 3, 27);
            var localDate2 =  new NodaTime.LocalDate();

            var diff = localDate2 - localDate1;
            WriteLine($"Difference: {diff.Days} days");
        }
    }
}
