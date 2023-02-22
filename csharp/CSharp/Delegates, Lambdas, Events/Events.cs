using System;

namespace CSharp;

public static class Events
{
    public static void Start()
    {
        var p = new Person("Bill");
        p.Shout = Person_Shouts;
        p.Shout += Person_BangsHead;

        p.Poke();
        p.Poke();
        p.Poke();
        p.Poke();
    }

    private class Person
    {
        public string Name { get; init; }
        public uint AngerLevel;
        public EventHandler? Shout;

        public Person(string name)
        {
            Name = name;
        }

        public void Poke()
        {
            if (++AngerLevel >= 3)
            {
                Shout?.Invoke(this, EventArgs.Empty);
            }
        }
    }

    static void Person_Shouts(object? sender, EventArgs e)
    {
        if (sender is null)
            return;

        var p = (Person)sender;
        WriteLine($"{p.Name} is Level {p.AngerLevel} angry and is shouting!");
    }

    static void Person_BangsHead(object? sender, EventArgs e)
    {
        if (sender is null)
            return;

        var p = (Person)sender;
        WriteLine($"{p.Name} banged their head against a wall!");
    }
}