using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;
using System.Threading.Tasks;

// TODO: Add to the main menu
namespace CSharp
{
    public static class Covariance
    {
        public static void Start()
        {

        }

        public interface IMatchValue<T>
        {
            T Value { get; set; }
        }

        public class FirstValue<T> : IMatchValue<T>
        {
            public T Value { get; set; }

            public FirstValue(T value)
            {
                Value = value;
            }

            //public SecondValue<T> Process()
            //{
            //    T newValue = default;

            //    if (Value is int i)
            //        newValue = i + 5;

            //    return new SecondValue<T> { Value = this.Value + 5 }
            //}
        }

        public class SecondValue<T> : IMatchValue<T>
        {
            public T Value { get; set; }

            public SecondValue(T value)
            {
                Value = value;
            }
        }
    }
}