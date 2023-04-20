namespace CSharp;

public static class Program
{
    public static readonly Dictionary<string, Action> Commands =
        new()
        {
            { "quick", () => QuickTesting.Start() },

            // Generics
            { "generics", () => Generics.Start() },
            { "generics2", () => Generics2.Start() },

            // Console
            { "keys", () => Keys.Start() },
            { "consolecolor", () => ConsoleColors.Start() },
            { "fontsize", () => CLI.FontSize.Start() },

            // LINQ
            { "linq-join", () => Joins.Start() },
            { "linq-grouping", () => Grouping.Start() },
            { "aggregate", () => Aggregate.Start() },

            // Collections
            { "collections", () => Collections.Start() },
            { "sorteddictionary", () => SortedDictionary.Start() },
            { "lookups", () => Lookups.Start() },
            { "enumerating", () => Enumerating.Start() },
            { "immutablecollections", () => Immutable.Start() },
            { "yield", () => Yield.Start() },

            // Types
            { "dynamic", () => Dynamic.Start() },

            // By version
            { "cs9", () => VersionTesting.Version09.Start() },
            { "cs11", () => VersionTesting.Version11.Start() },

            // Regex
            { "regexgroups", () => RegexGroups.Start() },
            { "classpractice", () => ClassPractice.Start() },

            // Delegates, Lambdas, Events
            { "delegate", () => DelegateTypes.Start() },
            { "func", () => FuncAction.Start() },
            { "predicate-separation", () => PredicateSeparation.Start() },
            { "multiple-actions", () => MultipleActions.Start() },
            { "multiple-funcs", () => MultipleFuncs.Start() },
            { "events", () => Events.Start() },

            // Async
            { "await", () => AwaitPractice.Start() },
            { "await2", () => AwaitExample.Start() },
            { "whenany", async () => await WhenAny.Start() },
            { "threads", () => Threads.Start() },
            { "task-load", () => TaskLoad.Start() },
            { "proper-async", async () => await ProperlyDoneAsync.Start() },
            { "parallel-foreach", () => ParallelForEachAsync.StartAsync().Wait() },

            // Concurrency
            { "asparallel", () => AsParallel.Start() },
            { "parallel", () => TaskParallelPractice.MyTasks() },
            { "parallel-loop", () => TaskParallelPractice.Looping() },

            // API
            { "api", () => RestAPI.Start() },

            // Dates and times
            { "datetimeparseexact", () => DateTimeParseExact.Start() },
            { "datetimeparse", () => DateTimeParse.Start() },
            { "nodatime-basic", () => NodaTimeBasic.Start() },

            // Misc
            { "numberstotal", () => NumbersTotal.Start() },
            { "intervals", () => Intervals.Start() },
            { "sudoku", () => SudokuValidator.Start() }, // いる？
            { "iterate-properties", () => PropertyIteration.Start() },
            { "methodchaining", () => MethodChaining.Start() },
            { "cancel", () => Cancellation.Start() },
            { "variables", () => Variables.Start() },
            { "compare", () => Comparison.Compare() },
            { "rot", () => Rot.Start() },
            { "reddit", () => RedditDailyProgrammer.Start()},
            { "equality", () => Equality.Start() },

            // IO
            { "filehandling", () => FileHandling.Start() },

            // Linguistics
            { "tm", () => TranslationMemory.Start() },

            // Attributes
            { "callerargumentexpression", () => CallerArgumentExpressionAttributeTest.Start()},

            // Puzzles and such
            { "number2", () => Challenges.AlphabeticNumberPuzzleSolver.Run() },
            { "10interview", () => Challenges.TenCommonJavaScriptQuestions.Start() },

            // NuGet packages and such
            { "oneof", () => NuGetPackages.NugetOneOf.Start() },
            { "functional", () => NuGetPackages.FunctionalCSharp.Start() },
            { "functional2", () => Functional.FunctionalCSharp.Start() },
        };

    public static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            WriteLine("You must enter a command. Use one of the following:");
            WriteLine("→ " + string.Join(", ", Commands.Keys));
            return;
        }

        var command = args[0].ToLowerInvariant().Trim();

        if (Commands.ContainsKey(command))
        {
            Commands[command]();
        }
        else
        {
            WriteLine("Invalid command. Use one of the following:");
            WriteLine("→ " + string.Join(", ", Commands.Keys));
        }
    }
}
