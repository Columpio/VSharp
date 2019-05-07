using NUnit.Framework;
using System;

namespace VSharp.Test.Tests
{

//    public class HelloClass
//    {
//        public static string ReturnHello()
//        {
//            return "Hello";
//        }
//    }

    [TestSvmFixture]
    public sealed class Strings
    {
        // Expecting HeapRef on empty string
        [TestSvm]
        public static string EmptyString(int n, int m)
        {
            return String.Empty;
        }

        [TestSvm]
        public static int GetConcreteHash()
        {
            String str = "sample string";
            return str.GetHashCode();
        }

        [TestSvm]
        public static int GetSymbolicHash(string a)
        {
            return a.GetHashCode();
        }

        [TestSvm]
        public static string SymbolicString(string s)
        {
            var len = s.Length;
            return s;
        }

        [TestSvm]
        public static int NullLength()
        {
            string s = null;
            return s.Length;
        }

        [TestSvm]
        public static string HopHeyCharArray(char[] a)
        {
            return new string(a);
        }

        [TestSvm]
        public static string ConcreteIsInterned()
        {
            String a = "interned";
            return String.IsInterned(a);
        }

        [TestSvm]
        public static string ConcreteIntern()
        {
            string a = new string(new char[] {'a', 'b', 'c'});
            return String.Intern(a);
        }

        [TestSvm]
        public static Object NotInterned()
        {
            string a = new string(new char[] {'a', 'b', 'c'});
            return String.IsInterned(a);
        }

        // TODO: can be equals, but result lies! (How to fix -- Strings.fs : 70)
//        public static bool HashEquals(char[] test)
//        {
//            String str = new string(test);
//            String str2 = "equals?";
//            return str.GetHashCode() == str2.GetHashCode();
//        }

        // TODO: too big guards
//        public static string SymbolicIsInterned(string a)
//        {
//            return String.IsInterned(a);
//        }

        // TODO: doesn't work yet (How to fix -- MemoryCell.fs : 5, Memory.fs : 364)
//        public static String ConditiononalIntern(bool g)
//        {
//            string a = new string(new char[] {'a', 'b', 'c'});
//            if (g)
//            {
//                String.Intern(a);
//            }
//            string b = new string(new char[] {'a', 'b', 'c'});
//            return String.Intern(b);
//        }

        // TODO: doesn't work yet (How to fix -- make interning pool more like hash table)
//        public string CrossClassInternLiterals() // TODO: One possible way -- update poolKey by dereferencing value
//        {
//            string str = "Hello";
//            //Class2.Print(str);
//            Console.WriteLine(str);
//            unsafe
//            {
//                fixed (char* ch = "Hello")
//                {
//                    ch[0] = 'W';
//                }
//            }
//            return HelloClass.ReturnHello();
//        }
    }
}
