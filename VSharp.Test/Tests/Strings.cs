using NUnit.Framework;
using System;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public sealed class Strings
    {
        // Expecting HeapRef on empty string
        [TestSvm]
        public static string EmptyString(int n, int m)
        {
            return String.Empty;
        }
    }
}
