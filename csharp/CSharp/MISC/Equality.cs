namespace CSharp
{
    public static class Equality
    {
        public static void Start()
        {
            StringEqualityTest();
        }

        private static void StringEqualityTest()
        {
            string s1 = "test";
            string s2 = "test";
            string s3 = "test1"[..4];
            object s4 = s3;

            WriteLine($"{object.ReferenceEquals(s1, s2)} {s1 == s2} {s1.Equals(s2)}");
            WriteLine($"{object.ReferenceEquals(s1, s3)} {s1 == s3} {s1.Equals(s3)}");
#pragma warning disable CS0253 // "Possible unintended reference comparison"
            WriteLine($"{object.ReferenceEquals(s1, s4)} {s1 == s4} {s1.Equals(s4)}");
#pragma warning restore CS0253
        }
    }
}
