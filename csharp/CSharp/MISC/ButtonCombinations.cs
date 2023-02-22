using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;


namespace CSharp
{
    // C# program to print all possible strings of length k 
    // https://www.geeksforgeeks.org/print-all-combinations-of-given-length/

    public static class ButtonCombinations
    {
        //static List<string> permutations = new List<string>();
        // static new ValueTuple<char, char, char, char> permutation = new ValueTuple<char, char, char, char>();

        // public static ValueTuple<char, char, char, char> GetPINs(string observedButtons)
        // {
        //     //var permutations = new List<string>();
        //     //var permutation = new ValueTuple<char, char, char, char>();

        //     for (var i = observedButtons.Length-1; i >= 0; i--)
        //     {
        //         //foreach ()
        //     }

        //     return permutations;
        // }

        // static char GetAdjacentButtons(char button, int position)
        // {
        //     foreach (var ch in adjacentButtons[button])
        //     {
                
        //     }
        //     return '';
        // }

        public static IReadOnlyDictionary<char, List<char>> adjacentButtons =
            new Dictionary<char, List<char>>
            {
                ['0'] = new List<char> { '0', '8' },
                ['1'] = new List<char> { '1', '2', '4' },
                ['2'] = new List<char> { '1', '2', '3', '5' },
                ['3'] = new List<char> { '2', '3', '6' },
                ['4'] = new List<char> { '1', '4', '5', '7' },
                ['5'] = new List<char> { '2', '4', '5', '6', '8' },
                ['6'] = new List<char> { '3', '5', '6', '9' },
                ['7'] = new List<char> { '4', '7', '8' },
                ['8'] = new List<char> { '5', '7', '8', '9', '0' },
                ['9'] = new List<char> { '6', '8', '9' },
            };
        
        static List<string> results = new List<string>();
    }
}