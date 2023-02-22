using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace CSharp
{
    // C# program to print all possible strings of length k
    // https://www.geeksforgeeks.org/print-all-combinations-of-given-length/

    /*
    public static class CharacterCombinations
    {
        // Driver Code
        public static void CharacterCombinationCheck()
        {
            WriteLine("First Start");
            char[] set1 = {'0', '1', '2', '3'};
            int k = 3;
            PrintAllKLength(set1, k);
            // foreach (var result in results)
            //     Write(result + ",");
            // WriteLine(" -- done!");
        }

        // The method that prints all possible strings of length k.
        // It is mainly a wrapper over recursive function PrintAllKLengthRec()
        static void PrintAllKLength(char[] set, int k)
        {
            int n = set.Length;
            PrintAllKLengthRec(set, "", n, k);
        }

        // The main recursive method to print all possible strings of length k
        private static void PrintAllKLengthRec(char[] set, string prefix, int n, int k)
        {
            // Base case: k is 0, print prefix
            if (k == 0)
            {
                //WriteLine(prefix);
                int total = 0;
                foreach (var num in prefix)
                {
                    var thisNum = Convert.ToInt32(Char.GetNumericValue(num));
                    total += thisNum;
                }
                if (total == 2)
                    results.Add(prefix);  // Original line
                return;
            }

            // One by one add all characters from set and recursively call for k equals to k-1
            for (int i = 0; i < n; ++i)
            {
                // Next character of input added
                String newPrefix = prefix + set[i];

                // k is decreased, because we have added a new character
                PrintAllKLengthRec(set, newPrefix, n, k - 1);
            }
        }
    }
    */
}
