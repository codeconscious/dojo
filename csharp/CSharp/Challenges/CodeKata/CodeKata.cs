#nullable disable

namespace CSharp
{
    public static class SudokuValidator
    {
        public static void Start()
        {
            WriteLine(ValidateSolution());
        }

        private static bool ValidateSolution()
        {
            // var sudokuMatrix = new SudokuMatrix
            // {
            //     Matrix = new int[][] {
            //         new int[] {5, 3, 4, 6, 7, 8, 9, 1, 2},
            //         new int[] {6, 7, 2, 1, 9, 5, 3, 4, 8},
            //         new int[] {1, 9, 8, 3, 4, 2, 5, 6, 7},
            //         new int[] {8, 5, 9, 7, 6, 1, 4, 2, 3},
            //         new int[] {4, 2, 6, 8, 5, 3, 7, 9, 1},
            //         new int[] {7, 1, 3, 9, 2, 4, 8, 5, 6},
            //         new int[] {9, 6, 1, 5, 3, 7, 2, 8, 4},
            //         new int[] {2, 8, 7, 4, 1, 9, 6, 3, 5},
            //         new int[] {3, 4, 5, 2, 8, 6, 1, 7, 9}
            //     }
            // };

            var matrix = new int[][] {
                new int[] {1, 2, 3, 4, 5, 6, 7, 8, 9},
                new int[] {2, 3, 1, 5, 6, 4, 8, 9, 7},
                new int[] {3, 1, 2, 6, 4, 5, 9, 7, 8},
                new int[] {4, 5, 6, 7, 8, 9, 1, 2, 3},
                new int[] {5, 6, 4, 8, 9, 7, 2, 3, 1},
                new int[] {6, 4, 5, 9, 7, 8, 3, 1, 2},
                new int[] {7, 8, 9, 1, 2, 3, 4, 5, 6},
                new int[] {8, 9, 7, 2, 3, 1, 5, 6, 4},
                new int[] {9, 7, 8, 3, 1, 2, 6, 4, 5}
            };

            if (!IsSquareArray())
                return false;

            return AreValuesValid();


            bool IsSquareArray()
            {
                //var matrix = sudokuMatrix.Matrix;

                if (!matrix.All(r => r.Length == matrix.First().Length) || // all rows same length
                    matrix.First().Length != matrix.Length || // row length == row count
                    matrix.SelectMany(row => row).ToArray().Any(i => i < 1 || i > 9))
                        return false;

                return true;
            }

            bool AreValuesValid()
            {
                //var matrix = sudokuMatrix.Matrix;

                // Check rows
                if (matrix.Any(r => r.Distinct().Count() != r.Length))
                    return false;

                // Check columns
                for (var i = 0; i < matrix.Length; i++)
                {
                    var columnArray = new List<int>();
                    foreach (var row in matrix)
                    {
                        // System.WriteLine(string.Join(", ", row));
                        columnArray.Add(row[i]);
                    }
                    WriteLine($"Column array #{i}: {string.Join(",", columnArray)}.");
                    if (columnArray.Distinct().Count() != columnArray.Count())
                        return false;
                }

                WriteLine("All okay!");
                return true;
            }

        }
    }

    public class SudokuMatrix
    {
        public int[][] Matrix { get; set; }
    }
}