//using Spectre.Console;
using System.IO;
using System.Runtime.CompilerServices;

namespace CSharp;

public static class QuickTesting
{
    public static void Start()
    {
        // var ch = 'a';
        // WriteLine(ch + "@" + (int) ch + " --   " + Convert.ToString(ch, 2));
        // var ch2 = ch >> 1;
        // WriteLine((char) ch2 + "@" + ch2 + " -- " + Convert.ToString(ch2, 2));

        // const int number = 5;
        // Write(number + ", ");
        // WriteLine(Convert.ToString(number, 2));
        // int newNumber = number << 2;
        // Write(newNumber + ", ");
        // WriteLine(Convert.ToString(newNumber, 2));

        // WriteLine(FindInvalidChars("/Volumes/BLACK-4TB/Media/Music/AiPod Classic/5 Stars/ClariS - 魔法少女まどか☆マギカ - 01 - コネクト.m4a"));
        // WriteLine(File.Exists("/Volumes/BLACK-4TB/Media/Music/AiPod Classic/5 Stars/ClariS - 魔法少女まどか☆マギカ - 01 - コネクト.m4a") ? "YES" : "NO");

        PatternMatchingTest();
    }

    private static string FindInvalidChars(string text)
    {
        var invalidChars = new StringBuilder(text.Length);
        text.ForEach(c =>
        {
            if (Path.GetInvalidPathChars().Contains(c))
                invalidChars.Append(c);
        });
        return invalidChars.ToString();
    }

    /// <summary>
    /// https://www.reddit.com/r/csharp/comments/10luglo/net_7_new_list_pattern/
    /// </summary>
    private static void PatternMatchingTest()
    {
        // var list = new List<int>() { 1, 2, 3, 4 };
        var list = Enumerable.Range(0, 100).ToList();

        var format = list switch
        {
                null => "This is null!",  // Nullable == true
                [] => "No item was found",  // Empty
                [var singleItem] => $"ItemFound = {singleItem}",  // One item found
                [var _, var _, var _] => "Found 3 items",  // Check for 3 items
                { Count: 100 } => "100 items!",  // Another type for count check
                _ => throw new Exception()  // Default case
        };

        WriteLine("format == " + format);
    }
}
