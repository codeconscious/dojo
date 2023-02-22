namespace CSharp
{
    public static class Rot
    {
        public static void Start()
        {
            var offsets = RotOffsetRepository.GetRotOffsets;

            const string input = "Hello. こんにちは。你好。안녕하세요!";
            const string expectedOutput = "Uryyb. まとゖれい。砸艕。꽶윧뾆휊냂!";
            var output = new StringBuilder();

            foreach (var ch in input)
            {
                var relevantOffset = offsets.SingleOrDefault(o =>
                    RotUtilities.IsWithinRange(ch, o.RangeStart, o.RangeEnd));

                if (relevantOffset is null)
                {
                    output.Append(ch);
                    continue;
                }

                output.Append(ch < relevantOffset.RangeStart + relevantOffset.OffsetAmount
                    ? Convert.ToChar(ch + relevantOffset.OffsetAmount)
                    : Convert.ToChar(ch - relevantOffset.OffsetAmount));
            }

            WriteLine((expectedOutput == output.ToString() ? "SUCCESS: " : "FAILURE: ") + output.ToString());
        }
    }


    public static class RotUtilities
    {
        public static bool IsWithinRange(uint textNumber, uint rangeMin, uint rangeMax)
        {
            return textNumber >= rangeMin && textNumber <= rangeMax;
        }
    }

    // TODO: Add logic to determine if the range is even and, thus, valid?
    public record RotOffset(uint RangeStart, uint RangeEnd, uint OffsetAmount);

    public static class RotOffsetRepository
    {
        // TODO: Add versions for later updates?
        private static readonly List<RotOffset> RotOffsets = new() {  // TODO: Refactor to calcuate the offsets?

            // Kanji (Chinese and Japanese)
            new RotOffset(0x4E00, 0x9FAF, 10456), /* Kanji */

            // Japanese & English
            new RotOffset(0x3041, 0x3096, 43) /* Hiragana */,
            new RotOffset(0x30A1, 0x30FA, 45) /* Katakana (zenkaku) */,
            new RotOffset(0xFF66, 0xFF9D, 28) /* Katanaka (hankaku) */,
            new RotOffset(0x0041, 0x005A, 13) /* Uppercase (hankaku) */,
            new RotOffset(0xFF21, 0xFF3A, 13) /* Uppercase (zenkaku) */,
            new RotOffset(0x0061, 0x007A, 13) /* Lowercase (hankaku) */,
            new RotOffset(0xFF41, 0xFF5A, 13) /* Lowercase (zenkaku) */,
            new RotOffset(0x0030, 0x0039, 5) /* Numbers (hankaku) */,
            new RotOffset(0xFF10, 0xFF19, 5) /* Numbers (zenkaku) */,

            // Korean (I don't actually speak Korean, so I hope this makes sense!)
            new RotOffset(0xAC00, 0xD7A3, 5586), // Hangul syllables
            new RotOffset(0x1100, 0x11FF, 128), // Hangul jamo
            new RotOffset(0x3130, 0x318F, 48), // Hangul compatibility jamo
            new RotOffset(0xA960, 0xA97F, 16), // Hangul extended-A
            new RotOffset(0xD7B0, 0xD7FF, 40) // Hangul extended-B
        };

        public static List<RotOffset> GetRotOffsets => RotOffsets;
    }
}