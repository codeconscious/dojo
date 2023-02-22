using System;
namespace CSharp
{
    public static class Variables
    {
        public static void Start()
        {
            Add(1, 2, out uint result);
            WriteLine($"{result} is a(n) {result.GetType()}");
        }

        private static void Add(uint num1, uint num2, out uint result)
        {
            result = num1 + num2;
        }
    }
}
