namespace CSharp
{
    public static class RedditDailyProgrammer
    {
        public static void Start()
        {
            WriteLine(Challenge399("microspectrophotometries"));
        }

        /// <summary>
        /// Assign every lowercase letter a value, from 1 for a to 26 for z.
        /// Given a string of lowercase letters, find the sum of the values of the letters in the string.
        /// </summary>
        /// <url>https://www.reddit.com/r/dailyprogrammer/comments/onfehl/20210719_challenge_399_easy_letter_value_sum/</url>
        /// <param name="str"></param>
        private static int Challenge399(string str)
        {
            return str.Sum(letter => GetLetterValue(letter));

            static int GetLetterValue(char letter)
            {
                return letter switch
                {
                    >= 'a' and <= 'z' => (int) letter - 96,
                    _ => 0
                };
            }
        }
    }


}