using System.Threading.Tasks;

namespace CSharp;

public static class ParallelForEachAsync
{
    public static async Task StartAsync()
    {
        var nums = Enumerable.Range(0, 10).ToArray();

        WriteLine("Starting...");

        await Parallel.ForEachAsync(
            nums,
            new ParallelOptions { MaxDegreeOfParallelism = 3 },
            async (i, token) =>
        {
            WriteLine($"Starting iteration {i}");
            await Task.Delay(1000, token);
            WriteLine($"Finishing iteration {i}");
        });
    }
}