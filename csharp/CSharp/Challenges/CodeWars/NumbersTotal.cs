using System.Linq;
using System;
using System.Collections.Generic;

namespace CSharp
{
    // https://www.codewars.com/kata/5877e7d568909e5ff90017e6/csharp
    public static class NumbersTotal
    {
        public static void Start()
        {
            foreach (var num in FindAll(10, 3))
                WriteLine(num);
        }

        public static List<long> FindAll(int sumDigits, int numDigits)
        {
            // var low = (int) Math.Pow(10, numDigits-1);
            // var high = (int) Math.Pow(10, numDigits)-1;
            var low = int.Parse("1" + new string('0', numDigits-1));
            var high = int.Parse("1" + new string('0', numDigits))-1;

            var numbers = Enumerable
                .Range(low, high)
                .Where(n => AreNumbersOrderedAscending(n) &&
                            DoesSumMatch(n, sumDigits));

            //WriteLine(numbers.Count());

            if (!numbers.Any())
                return new List<long>();

            return new List<long> {
                numbers.Count(),
                numbers.Min(),
                numbers.Max()
            };
        }

        private static bool AreNumbersOrderedAscending(int n)
        {
            var numAsText = n.ToString();

            var i = 1;
            while (i < numAsText.Length)
            {
                if (numAsText[i-1] > numAsText[i])
                {
                    //result = false;
                    return false;
                }
                i++;
            }

            return true;
        }

        private static bool DoesSumMatch(int n, int sumDigits)
        {
            // https://stackoverflow.com/a/478974/11767771
            var sum = 0;
            while (n != 0)
            {
                sum += n % 10;
                n /= 10;
            }
            return sum == sumDigits;
        }
    }
}