using System;
using System.Linq;
using System.Collections.Generic;

namespace CSharp
{
    public static class FuncAction
    {
        public delegate void StringDelegate(string s1, string s2);

        public static void Start()
        {
            const string a = "Former";
            const string b = "Latter";

            // A local function can be used here.
            Func<string, string, string> combineFunc =
                (string aa, string bb) =>
                    "[Func] A: " + aa + " / B: " + bb;
            WriteLine(combineFunc(a, b));

            // A local function can be used here.
            Action<string,string> combineAction =
                (string aa, string bb) =>
                    WriteLine("[Action] A: " + aa + " / B: " + bb);
            combineAction(a, b);

            StringDelegate stringDelegate = CombineMethodOne;
            stringDelegate += CombineMethodTwo;
            stringDelegate("Jon", "Osterman");

            Action<string, string> action = CombineMethodOne;
            action("Action", "Time!");

            // A local function can be used here.
            Predicate<int> predicate = number => number > 10;
            WriteLine(predicate(20));
        }

        public static void CombineMethodOne(string aa, string bb)
        {
            WriteLine("[Method 1] A: " + aa + " / B: " + bb);
        }

        public static void CombineMethodTwo(string aa, string bb)
        {
            WriteLine("[Method 2] A: " + aa + " + B: " + bb);
        }

        // public static void PassDelegate(StringDelegate stringDelegate, string s1, string s2)
        // {
        //     WriteLine()
        // }
    }

    public static class DelegateTypes
    {
        public static void Start()
        {
            var writer = new Action<string>(WriteLine);
            var writer2 = new Action<string>(s => WriteLine(s));
            var capitalizer = new Func<string, string>(s => s.ToUpper());
            var isCapitalized = new Predicate<string>(input => input == input.ToUpper());
            var isNumericString = new Func<string, int, bool>((s, i) => s == i.ToString());

            writer("Hi");
            writer2("Hi");
            writer(capitalizer("good day, folks!"));

            var saitamaLower = isCapitalized("saitama");
            writer2($"This text {(saitamaLower ? "IS" : "is NOT")} capitalized.");
            var saitamaCap = isCapitalized("SAITAMA");
            writer2($"This text {(saitamaCap ? "IS" : "is NOT")} capitalized.");

            var n1 = isNumericString("three", 1);
            writer2($"This text {(n1 ? "IS" : "is NOT")} numerically identical.");
            var n2 = isNumericString("5", 5);
            writer2($"This text {(n2 ? "IS" : "is NOT")} numerically identical.");

            // Modifier modifer;
            // if (true)
            //     modifer = (s) => s.ToUpper();
            // else
            //     modifer = (s) => s.ToLower();
        }

        public delegate string Modifier(string input);
    }
}
