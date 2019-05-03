using System;

namespace VSharp.Test.Tests
{
    public sealed class Strings
    {
        // Expecting HeapRef on empty string
        public static string EmptyString(int n, int m)
        {
            return String.Empty;
        }
    }
}
