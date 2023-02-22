using System.Runtime.CompilerServices;

namespace CSharp
{
    public static class CallerArgumentExpressionAttributeTest
    {
        public static void Start()
        {
            var func = new Func<int, int, int>((j, k) => j + k);
            int a = 14, b = 44;

            CallerArgumentExpression(func(a, b));
        }

        private static void CallerArgumentExpression(int number, [CallerArgumentExpression("number")] string? message = null)
        {
            WriteLine($"{number} came from \"{message ?? "null"}\"");
        }
    }
}