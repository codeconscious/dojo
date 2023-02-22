namespace CSharp
{
    public static class Generics
    {
        private interface IPerson
        {
            Type Type { get; }
        }

        private class Person<T> : IPerson
        {
            public T? FavoriteThing { get; set; }
            public Type Type => typeof(T);
        }

        public static void Start()
        {
            IReadOnlyList<IPerson> people = new List<IPerson>
            {
                new Person<string> { FavoriteThing = "nature" },
                new Person<bool> { FavoriteThing = false },
                new Person<Color> { FavoriteThing = Color.Yellow },
                new Person<uint> { FavoriteThing = 15 }
            };

            foreach (var item in people)
            {
                if (item.Type == typeof(string))
                    ResultPrinter.Print<string>(item);
                if (item.Type == typeof(bool))
                    ResultPrinter.Print<bool>(item);
                if (item.Type == typeof(Color))
                    ResultPrinter.Print<Color>(item);
                if (item.Type == typeof(uint))
                    ResultPrinter.Print<uint>(item);
            }

            // var helper = new ResultPrinter();
            // helper.Print<string>(p1);
        }

        private enum Color { Red, Yellow };

        private static class ResultPrinter
        {
            public static void Print<T>(IPerson p)
            {
                WriteLine(((Person<T>) p).FavoriteThing!.ToString());
            }
        }
    }
}