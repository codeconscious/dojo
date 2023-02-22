using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.IO;

namespace CSharp
{
    public static class FileHandling
    {
        public static void Start()
        {
            try
            {
                CreateModifyFile("こんばんは。", "test.txt", false);
                ReadFile("test.txt");
            }
            catch (Exception ex)
            {
                WriteLine("ERROR: " + ex.Message);
            }
        }

        public static void CreateModifyFile(string textToWrite, string path, bool append = true)
        {
            if (string.IsNullOrWhiteSpace(textToWrite))
            {
                WriteLine("Nothing to write.");
                return;
            }

            using var writer = new StreamWriter(path, append);
            writer.WriteLine(textToWrite);
            writer.Close();
        }

        public static void ReadFile(string path) // Path needed?
        {
            using var reader = new StreamReader(path);
            while (!reader.EndOfStream)
            {
                WriteLine(reader.ReadLine());
            }
            reader.Close();
        }
    }
}
