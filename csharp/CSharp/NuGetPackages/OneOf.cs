using OneOf;

namespace CSharp.Packages
{
    public static class NugetOneOf
    {
        public static void Start()
        {
            var results = new List<OneOf<Number, Error>>
            {
                GetNumberOrError(-3, "Minus Three"),
                GetNumberOrError(6, "Six")
            };

            foreach (var result in results)
            {
                WriteLine(
                    result.Match(
                        num => $"{num.Value} is {num.Name}",
                        err => err?.Message ?? "Unknown error"));
            }
		}

        private static OneOf<Number, Error> GetNumberOrError(int value, string name)
        {
            return value switch
            {
                < 0 => new Error("Must be >= 0"),
                _ => new Number(value, name)
            };
        }

        private record Number(int Value, string Name);

        private record Error(string Message);
	}
}