using System;
using System.Linq;
using System.Collections.Generic;
using System.Reflection;

namespace CSharp
{
    public static class PropertyIteration
    {
        public static void Start()
        {
			var item = new
			{
				Name = "MyName",
				Age = 31,
				Alive = true,
				Quantum = (bool?)null,
				Thing = new Thang<uint>(3000, false),
				Group = new List<uint> { 3, 5, 30, 1048, 303777 }
			};

			Iterate(item);
		}

		private static void Iterate(object item, int indentation = 0, int levelsDeep = 1)
        {
			WriteLine($"Properties of \"{nameof(item)}\":");
			foreach (var prop in item.GetType().GetProperties())
			{
				WriteLine($"{new string(' ', indentation)}- {prop.Name}: {prop.GetValue(item, null) ?? "NULL"} ({prop.PropertyType})");

                if (levelsDeep < 3 && prop.IsNonStringEnumerable())
                {
					WriteLine("  (Is a list!)");

                    //Iterate(prop, indentation + 2, levelsDeep + 1);
                    if (prop is IList<object> propList)
                        foreach (var internalProp in propList)
                            Iterate(internalProp, indentation + 1, levelsDeep + 1);
                }
				else
                {
					WriteLine("  (Not a list.)");
                }
            }
		}

		public class Thang<T>
		{
			public T TypedValue { get; set; }
			public bool IsUsed { get; set; }

			public Thang(T typedValue, bool isUsed)
			{
				TypedValue = typedValue;
				IsUsed = isUsed;
			}
		}
	}
}