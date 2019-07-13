using NUnit.Framework;
using System;

namespace VSharp.Test.Tests
{
    using static RecursionUnrollingMode;

    [TestSvmFixture]
    public sealed class Strings
    {
        // Expecting HeapRef on empty string
        [TestSvm(SmartUnrolling, NeverUnroll)]
        public static string EmptyString(int n, int m)
        {
            return String.Empty;
        }

        [Ignore("term.equals: Stack overflow")]
        [TestSvm(SmartUnrolling)]
        public static string SymbolicString(string s)
        {
            var len = s.Length;
            return s;
        }

        [Ignore("term.equals: Stack overflow")]
        [TestSvm(SmartUnrolling)]
        public static int NullLength()
        {
            string s = null;
            return s.Length;
        }

        [TestSvm(SmartUnrolling, NeverUnroll)]
        public static string HopHeyCharArray(char[] a)
        {
            return new string(a);
        }
    }
}
