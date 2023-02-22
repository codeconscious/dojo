#nullable disable

namespace CSharp
{
    public static class Generics2
    {
        public static void Start()
        {
            IThing<int> it = new Thang<int> { Value = 50 };
            IThing<bool> it2 = new Stuff<bool> { Value = false };
            WriteLine(it.Output());
            WriteLine(it2.Output());
        }
    }

    public class Thang<T> : IThing<T> where T : struct
    {
        public T Value { get; set; }
        public Stack<T> ValueCollection { get; set; }

        public string Output() =>
            $"Thang's value is a {Value.GetType()}";
    }

    public class Stuff<T> : IThing<T> where T : struct
    {
        public T Value { get; set; }
        public Stack<T> ValueCollection { get; set; }

        public string Output() =>
            $"Stuff's value is a {Value.GetType()}";
    }

    public interface IThing<T> where T : struct
    {
        public T Value { get; set; }
        public Stack<T> ValueCollection { get; set; }

        public string Output();
    }
}