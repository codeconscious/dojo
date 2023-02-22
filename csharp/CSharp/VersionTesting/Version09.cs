using System;
using System.Collections.Generic;

namespace CSharp.VersionTesting
{
    public static class Version09
    {
        public static void Start()
        {
            var peeps = new List<Person>
            {
                new Person("うさぎ", 15),
                new Person("あみ", 15),
                new Person("まもる", 17)
            };

            peeps.Add(peeps[2] with { FirstName = "エンデミオン" });
            peeps.RemoveAt(2);

            foreach (var person in peeps)
                WriteLine($"{person.FirstName}は{person.Age}歳だ。");
        }
    }

    public record Person(string FirstName, ushort Age);
}
