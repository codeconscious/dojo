#nullable disable

using System;
using System.Linq;
using System.Collections;
using System.Collections.Generic;

// TODO: Add to the main menu
namespace CSharp
{
    public static class Generics3
    {
        public static void Start()
        {

        }
    }

    public enum ValueType { String, Integer, Decimal }

    public abstract class BaseMatch<T>
    {
        private T Value { get; }
    }

    public class ValueMatch<T> : BaseMatch<T> where T : struct
    {
        private T Value { get; set; } // Can we use base match instead?

        public ValueMatch(T value)
        {
            Value = value;
        }
    }

    public class CollectionMatch<T> : BaseMatch<T>
    {
        private T Value { get; set; }

        public CollectionMatch(T value)
        {
            Value = value;
        }
    }
}