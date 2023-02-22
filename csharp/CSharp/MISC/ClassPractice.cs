#nullable disable
using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;

namespace CSharp
{
    public static class ClassPractice
    {
        public static void Start()
        {
            var beings = new List<IDescribable>
            {
                new Being { GivenName = "Barry", FamilyName = "Pim" },
                new Human { GivenName = "Bruce", FamilyName = "Wayne", Ethnicity = Ethnicity.White, Gender = Gender.Male },
                new Mutant { GivenName = "Scott", FamilyName = "Summers", Ethnicity = Ethnicity.White, Gender = Gender.Male, Power = "Optic Blasts"},
                new Superhuman { GivenName = "Steven", FamilyName = "Rogers", Ethnicity = Ethnicity.White, Gender = Gender.Male, Power = "Peak Humanness" }
            };

            var humanoids = new List<Human>
            {
                new Human { GivenName = "Bruce", FamilyName = "Wayne", Ethnicity = Ethnicity.MiddleEastern, Gender = Gender.Female },
                new Mutant { GivenName = "Scott", FamilyName = "Summers", Ethnicity = Ethnicity.White, Gender = Gender.Male, Power = "Optic Blasts"},
                new Superhuman { GivenName = "Steven", FamilyName = "Rogers", Ethnicity = Ethnicity.White, Gender = Gender.Male, Power = "Peak Humanness" }
            };

            foreach (var being in beings)
            {
                WriteLine(being.Describe());
            }

            foreach (var h in humanoids)
            {
                WriteLine(h.OutputPowersStatement());
            }
        }

        public interface IDescribable
        {
            public string GivenName { get; set; }
            string FamilyName { get; set; }
            public bool HasPowers { get; set; }
            string Describe();
        }

        public class Being : IDescribable
        {
            public string GivenName { get; set; }
            public string FamilyName { get; set; }
            public bool HasPowers { get; set; }

            public Being() { HasPowers = false; }

            public virtual string Describe() => $"This being's name is {GivenName} of the {FamilyName} clan.";
            public virtual string OutputPowersStatement() => $"It {(HasPowers ? "does" : "does not")} have powers.";
        }

        public enum Ethnicity { Black, Asian, Latin, MiddleEastern, White, Other }
        public enum Gender { Male, Female, Other }

        public class Human : Being, IDescribable
        {
            public Ethnicity Ethnicity { get; set; }
            public Gender Gender { get; set; }

            public Human() { HasPowers = false; }

            public override string Describe() => $"A human that's {Gender} and {Ethnicity}.";

            public override string OutputPowersStatement() => $"{GetPronoun(true)} has no powers.";

            public virtual string GetPronoun(bool capitalize = false)
            {
                var pronoun = Gender switch
                {
                    Gender.Male => "he",
                    Gender.Female => "she",
                    _ => "they"
                };

                if (capitalize)
                    return char.ToUpper(pronoun[0]) + pronoun.Substring(1);

                return pronoun;
            }
        }

        interface IPoweredBeing
        {
            string Power { get; }
        }

        public class Mutant : Human, IPoweredBeing, IDescribable
        {
            public string Power { get; set; }

            public Mutant() { HasPowers = true; }

            public override string Describe()
            {
                return $"A mutant that's {Gender} and {Ethnicity} and that has the power of {Power}.";
            }
        }

        public class Superhuman : Human, IPoweredBeing, IDescribable
        {
            public string Power { get; set; }

            public Superhuman() { HasPowers = true; }

            public override string Describe()
            {
                return $"A superhuman that's {Gender} and {Ethnicity} and that has the power of {Power}.";
            }
        }
    }
}
