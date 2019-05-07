using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace VSharp.Test.Tests
{
    public sealed class ListNode
    {
        public int Key;
        public ListNode Next;
    }

    public sealed class A
    {
        public int Field;
        public int OtherField;
    }

    internal static class SharedList
    {
        public static ListNode CreateList(int n)
        {
            if (n <= 0)
                return null;
            ListNode tail = CreateList(n - 1);
            ListNode head = new ListNode {Key = 0, Next = tail};
            return head;
        }

        public static ListNode CreateDecreasingList(int n)
        {
            if (n <= 0)
                return null;
            ListNode tail = CreateDecreasingList(n - 1);
            ListNode head = new ListNode {Key = n, Next = tail};
            return head;
        }

        public static int Length(ListNode l)
        {
            if (l == null)
                return 0;
            return 1 + Length(l.Next);
        }

        public static int Last(ListNode l)
        {
            if (l == null)
                return -1;
            if (l.Next == null)
                return l.Key;
            return Last(l.Next);
        }

        public static int Sum(ListNode l)
        {
            if (l == null)
                return 0;
            return l.Key + Sum(l.Next);
        }

        public static ListNode Reverse(ListNode l)
        {
            if (l == null || l.Next == null)
                return l;
            var h = Reverse(l.Next);
            l.Next.Next = l; // l.Next is now the last element
            l.Next = null;
            return h;
        }

        public static void Crop(ListNode l, int n)
        {
            if (n <= 0 || l == null)
                return;
            if (n == 1)
            {
                l.Next = null;
                return;
            }

            Crop(l.Next, n - 1);
        }

        public static ListNode LastNode(ListNode l)
        {
            if (l == null)
                return null;
            if (l.Next == null)
                return l;
            return LastNode(l.Next);
        }

        public static void Append(ListNode l1, ListNode l2)
        {
            if (l1 == null)
                throw new ArgumentException();
            var l1Last = LastNode(l1);
            l1Last.Next = l2;
        }

        public static bool Contains(ListNode l, int k)
        {
            if (l == null)
                return false;
            if (l.Key == k)
                return true;
            return Contains(l.Next, k);
        }

        public static void IncN(ListNode l)
        {
            if (l == null)
                return;
            l.Key += 1;
            IncN(l.Next);
        }

        public static void IncNwithN(ListNode l, int n)
        {
            if (l == null || n == 0)
                return;
            l.Key += 1;
            IncNwithN(l.Next, n - 1);
        }

        public static void AtLeastHundreed(A a)
        {
            if (a == null)
                return;
            if (a.Field >= 100)
                return;
            a.Field++;
            AtLeastHundreed(a);
        }

        public static void Fact(A a)
        {
            if (a == null)
                return;
            if (a.Field < 2)
            {
                a.OtherField = 1;
                return;
            }

            var f = a.Field;
            a.Field--;
            Fact(a);
            a.OtherField *= f;
        }

        public static void JustSetField(A a)
        {
            if (a == null || a.Field == a.OtherField)
                return;
            a.Field = a.OtherField;
            JustSetField(a);
        }

        public static void StrangeSum(A a)
        {
            if (a == null)
                return;
            if (a.OtherField <= 0)
                return;
            a.Field += a.OtherField;
            a.OtherField--;
            StrangeSum(a);
        }

        public static void AddOther(A a, int n)
        {
            if (n <= 0)
                return;
            a.Field += a.OtherField;
            AddOther(a, n - 1);
        }
    }

    [TestSvmFixture]
    public static class ListWorking
    {
        [TestSvm]
        public static void LengthPositive(ListNode l)
        {
            if (SharedList.Length(l) < 0)
                throw new Exception();
        }

        [TestSvm]
        public static void LengthZero(ListNode l)
        {
            if (SharedList.Length(l) == 0 && l != null)
                throw new Exception();
        }

        [Ignore("Works (but very long!)")]
        static void IncConcreteList()
        {
            var l3 = new ListNode {Key = 30, Next = null};
            var l2 = new ListNode {Key = 20, Next = l3};
            var l1 = new ListNode {Key = 10, Next = l2};
            SharedList.IncN(l1);
            if (l3.Key != 31)
            {
                throw new Exception();
            }
        }

        [Ignore("Works (but very long!)")]
        static void IncConcreteListWithN(int n)
        {
            var l3 = new ListNode {Key = 30, Next = null};
            var l2 = new ListNode {Key = 20, Next = l3};
            var l1 = new ListNode {Key = 10, Next = l2};
            SharedList.IncNwithN(l1, n);
            if (l3.Key != 31)
            {
                throw new Exception();
            }
        }

        [TestSvm]
        public static void Test2(A a)
        {
            if (a == null)
                return;
            SharedList.AtLeastHundreed(a);
            if (a.Field < 100)
            {
                throw new Exception();
            }
        }

        [TestSvm]
        public static void Test3(A a)
        {
            if (a == null)
                return;
            var f = a.Field;
            SharedList.Fact(a);
            if (a.OtherField < f)
            {
                throw new Exception();
            }
        }

        [TestSvm]
        public static void JustCallTest(A a)
        {
            if (a == null)
                return;
            a.Field = 42;
            a.OtherField = 5;
            SharedList.JustSetField(a);
            if (a.Field != 5)
                throw new Exception();
        }

        [TestSvm]
        public static void JustCallTestSymbolic(A a)
        {
            if (a == null)
                return;
            int x = a.OtherField;
            SharedList.JustSetField(a);
            if (a.Field != x)
                throw new Exception();
        }

        [TestSvm]
        public static void StrangeSumTest(A a)
        {
            if (a == null)
                return;
            a.Field = 0;
            if (a.OtherField > 0)
            {
                SharedList.StrangeSum(a);
                if (a.Field < 0)
                    throw new Exception();
            }
        }

        [TestSvm]
        public static void LastTest(ListNode l)
        {
            if (SharedList.Length(l) == 0 && !(SharedList.Last(l) == -1))
                throw new Exception();
        }

        [TestSvm]
        public static void CreateLength(int n)
        {
            if (n >= 0 && SharedList.Length(SharedList.CreateList(n)) != n)
                throw new Exception();
        }

        [TestSvm]
        public static void LengthTest(ListNode l)
        {
            if (l == null)
                return;
            if (SharedList.Length(l) != SharedList.Length(l.Next) + 1)
                throw new Exception();
        }

        [TestSvm]
        public static void AddOtherTest(A a)
        {
            if (a == null)
                return;
            if (a.OtherField > 0 && a.Field == 0)
            {
                SharedList.AddOther(a, 3);
                if (a.Field <= 0)
                    throw new Exception();
            }
        }

        [TestSvm]
        public static void TestCreateDecreasingLast(int n)
        {
            if (n <= 0)
                return;
            var l = SharedList.CreateDecreasingList(n);
            int x = SharedList.Last(l);
            if (x != 1)
                throw new Exception();
        }

        [TestSvm]
        public static void SumAfterInc(ListNode l)
        {
            var s = SharedList.Sum(l);
            SharedList.IncN(l);
            var s2 = SharedList.Sum(l);
            if (!(s <= s2))
                throw new Exception();
        }

        [TestSvm]
        public static void TestReverseNull(ListNode l)
        {
            if (l != null && SharedList.Reverse(l) == null)
                throw new Exception();
        }

        [TestSvm]
        public static void TestCrop(ListNode l, int n)
        {
            if (n <= 0)
                return;
            SharedList.Crop(l, n);
            if (SharedList.Length(l) > n)
                throw new Exception();
        }

        [TestSvm]
        public static void TestReverseOfReverse(ListNode l)
        {
            if (l != SharedList.Reverse(SharedList.Reverse(l)))
                throw new Exception();
        }

        [TestSvm]
        public static void LastNodeAndReverse(ListNode l)
        {
            if (SharedList.LastNode(l) != SharedList.Reverse(l))
                throw new Exception();
        }

        [TestSvm]
        public static void LastNodeOfReverse(ListNode l)
        {
            if (l != SharedList.LastNode(SharedList.Reverse(l)))
                throw new Exception();
        }

        [TestSvm]
        public static void TestContainsLast(ListNode l)
        {
            if (l == null)
                return;
            if (!SharedList.Contains(l, SharedList.Last(l)))
                throw new Exception();
        }

        [TestSvm]
        public static void TestReverseLast(ListNode l)
        {
            if (l == null)
                return;
            var x = SharedList.Last(l);
            var y = SharedList.Reverse(l).Key;
            if (x != y)
                throw new Exception();
        }

        [TestSvm]
        public static void TestLastReverse(ListNode l)
        {
            if (l == null)
                return;
            var x = l.Key;
            var y = SharedList.Last(SharedList.Reverse(l));
            if (x != y)
                throw new Exception();
        }

        [TestSvm]
        public static void EqualLength(ListNode l1, ListNode l2)
        {
            if (l1 == l2 && SharedList.Length(l1) != SharedList.Length(l2))
                throw new Exception();
        }

        [TestSvm]
        public static void TestLengthOfReverse(ListNode l)
        {
            if (SharedList.Length(l) != SharedList.Length(SharedList.Reverse(l)))
                throw new Exception();
        }

        [TestSvm]
        public static void TestSumOfReverse(ListNode l)
        {
            if (SharedList.Sum(l) != SharedList.Sum(SharedList.Reverse(l)))
                throw new Exception();
        }

        [TestSvm]
        public static void TestCropByLength(ListNode l)
        {
            int n = SharedList.Length(l);
            SharedList.Crop(l, n);
            if (n != SharedList.Length(l))
                throw new Exception();
        }

        [TestSvm]
        public static void TestDoubleCrop(ListNode l, int n)
        {
            if (n <= 0)
                return;
            SharedList.Crop(l, n);
            int len = SharedList.Length(l);
            SharedList.Crop(l, n);
            if (SharedList.Length(l) != len)
                throw new Exception();
        }

        [TestSvm]
        public static void TestCreate(int n)
        {
            if (n > 1 && SharedList.CreateList(n).Next.Key > 0)
                throw new Exception();
        }

        [TestSvm]
        public static void TestSumDecreasing(int n) // nobody supports multiplication
        {
            if (n <= 0)
                return;
            var l = SharedList.CreateDecreasingList(n);
            var s = SharedList.Sum(l);
            if (2 * s != n * (n + 1))
                throw new Exception();
        }
    }

    public static class WIP
    {
        //TODO: CORE
        static void SumTest1() // doesn't work: HeapRef(HeapRef); missing argument
        {
            if (SharedList.Sum(SharedList.CreateList(10)) > 0)
                throw new Exception();
        }

        private static void Swap(A a, int n)
        {
            if (n <= 0)
                return;
            int x = a.Field;
            a.Field = a.OtherField;
            a.OtherField = x;
            Swap(a, n - 1);
        }

        static void Test4() // CORE: stack does not contain key (num, LocalVariable:-1283898937)!
        {
            var a = new A {Field = 3, OtherField = 5};
            Swap(a, 50);
            if (a.Field != 15)
                throw new Exception();
        }

        static void TestReverseDecreaseHead(int n) // CORE: empty unions
        {
            if (n <= 0)
                return;
            var l = SharedList.CreateDecreasingList(n);
            var r = SharedList.Reverse(l);
            if (r.Key != 1)
                throw new Exception();
        }

        static void TestLengthAppend(ListNode l1, ListNode l2) // CORE: stack doesn't contain key
        {
            if (l1 == null)
                return;
            var l1Length = SharedList.Length(l1);
            var l2Length = SharedList.Length(l2);
            SharedList.Append(l1, l2);
            var appLength = SharedList.Length(l1);
            if (appLength != l1Length + l2Length) // not right: cycles
                throw new Exception();
        }

        static void TestAppendCreate(int n, int m) // CORE: stack doesn't contain key
        {
            if (n <= 0)
                return;
            var l1 = SharedList.CreateList(n);
            var l2 = SharedList.CreateList(m);
            SharedList.Append(l1, l2);
            if (SharedList.Length(l1) != n + m)
                throw new Exception();
        }
    }

    [TestSvmFixture]
    public class Lists
    {
//
//        private int a = 0;
//
//        private bool DoSmth()
//        {
//            a += 1;
//            return a > 3;
//        }

        [TestSvm]
        public bool Construct()
        {
            var a = new List<int>(4) { 1, 2, 3, 4 };
            var b = new int[4, 1];
            var c = new int[4] { 5, 6, 7, 8 };
            return a.Count == b.Length && b.Length == c.Length && c.Length == c[3] - 4;
        }

        [TestSvm]
        public int[] Mutate(int i)
        {
            var a = new int[] {1, 2, 3, 4, 5};
            a[i] = 10;
            return a;
        }

        [TestSvm]
        public int LowerBoundTest()
        {
            var c = new int[4, 2] { { 1, 1 }, { 2, 2 }, { 3, 3 }, { 4, 4 } };
            return c.GetLowerBound(1);
        }

        [TestSvm]
        public int LowerBoundExceptionTest(int[,] array)
        {
            return array.GetLowerBound(2);
        }

        [TestSvm]
        public int LowerBoundSymbolicTest(int[,] array, int dimension)
        {
            return array.GetLowerBound(dimension);
        }

        [TestSvm]
        public int UpperBoundTest()
        {
            var c = new int[4, 2] { { 1, 1 }, { 2, 2 }, { 3, 3 }, { 4, 4 } };
            return c.GetUpperBound(0);
        }

//        public void ClearTest()
//        {
//            var a = new int[4] { 5, 6, 7, 8 };
//            SystemArray.Clear(a, 1, 2);
//        }
//
//        public void Copy()
//        {
//            var a = new int[4] { 5, 6, 7, 8 };
//            var b = new int[3];
//            a.CopyTo(b, 1);
//        }

        [TestSvm]
        public int RankTest()
        {
            var c = new int[4, 2] { { 1, 1 }, { 2, 2 }, { 3, 3 }, { 4, 4 } };
            return c.Rank;
        }

        [TestSvm]
        public static int[] RetOneDArray1(bool flag1, bool flag2)
        {
            int[] arr = new int[5];
            if (flag1)
            {
                arr[1] = 42;
            }
            else if (flag2)
            {
                arr[1] = 89;
            }
            return arr;
        }

        [TestSvm]
        public static int[] RetOneDArray2(int n)
        {
            int[] arr = new int[n];
            if (n == 5)
            {
                arr[4] = 99;
                arr[1] = 42;
            }
            if (n == 8)
            {
                arr[1] = 89;
                arr[7] = 66;
            }
            return arr;
        }

        [TestSvm]
        public static Array RetSystemArray1(Array arr)
        {
            if (arr is int[])
            {
                var arrOne = arr as int[];
                arrOne[1] = 5;
            }
            else if (arr is int[,])
            {
                var arrOne = arr as int[,];
                arrOne[1,1] = 7;
            }
            return arr;
        }

        [TestSvm]
        public static Array RetSystemArray2(Array arr)
        {
            if (arr is int[])
            {
                var arrOne = arr as int[];
                arrOne[1] = 5;
            }
            if (arr is int[,])
            {
                var arrOne = arr as int[,];
                arrOne[1,1] = 7;
            }
            if (arr is int[,,])
            {
                var arrOne = arr as int[,,];
                arrOne[1,1,1] = 42;
            }
            return arr;
        }
    }
}
