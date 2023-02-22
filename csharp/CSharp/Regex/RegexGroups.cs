using System.Linq;
using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace CSharp
{
    public static class RegexGroups
    {
        public static void Start()
        {
            const string text = "Super Mario entered the warp pipe and emerged on the Star Road!";
            const string pattern = @"(?<eAndSpace>e\s)|(?<letterA>a)";

            var matches = Regex.Matches(text, pattern);

            WriteLine("Count: " + matches.Count);
            foreach (Match match in matches)
            {
                var groups = match.Groups;
                WriteLine("   Groups: " + groups.Count);

                for (var i = 0; i < groups.Count; i++)
                {
                    WriteLine($"      Group #{i}: {groups[i]} - " +
                        $"Index == {groups[i].Index}; " +
                        $"Length == {groups[i].Length}; " +
                        $"Name == {groups[i].Name}; " +
                        $"Success: {groups[i].Success}");
                }
            }
        }
    }
}