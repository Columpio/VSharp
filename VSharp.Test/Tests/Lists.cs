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


    public sealed class BinTreeNode
    {
        public int Key;
        public BinTreeNode Left = null;
        public BinTreeNode Right = null;

        public BinTreeNode(int x)
        {
            Key = x;
        }

        public void Add(int x)
        {
            if (Key == x)
                return;
            if (x < Key)
            {
                if (Left == null)
                    Left = new BinTreeNode(x);
                else
                    Left.Add(x);
            }
            else
            {
                if (Right == null)
                    Right = new BinTreeNode(x);
                else
                    Right.Add(x);
            }
        }

        public bool Contains(int x)
        {
            if (Key == x)
                return true;
            if (x < Key)
            {
                if (Left == null)
                    return false;
                else
                    return Left.Contains(x);
            }
            else
            {
                if (Right == null)
                    return false;
                else
                    return Right.Contains(x);
            }
        }
    }

    public sealed class BinTree
    {
        private BinTreeNode _root = null;
        public int Key => _root.Key;

        public void Add(int x)
        {
            if (_root == null)
                _root = new BinTreeNode(x);
            else
                _root.Add(x);
        }

        public bool Contains(int x)
        {
            if (_root == null)
                return false;
            else
                return _root.Contains(x);
        }
    }

    public sealed class A
    {
        public int Field;
        public int OtherField;
    }

    internal static class SharedTree
    {
        public static BinTreeNode Add(BinTreeNode tree, int x)
        {
            if (tree == null)
                return new BinTreeNode(x);
            if (x < tree.Key)
                tree.Left = Add(tree.Left, x);
            else if (x > tree.Key)
                tree.Right = Add(tree.Right, x);

            return tree;
        }

        public static bool Contains(BinTreeNode tree, int x)
        {
            if (tree == null)
                return false;
            if (tree.Key == x)
                return true;
            if (x < tree.Key)
                return Contains(tree.Left, x);
            return Contains(tree.Right, x);
        }

        public static BinTreeNode FromList(ListNode list)
        {
            if (list == null)
                return null;
            var tree = FromList(list.Next);
            if (tree == null)
                return new BinTreeNode(list.Key);
            Add(tree, list.Key);
            return tree;
        }

        public static int Max(BinTreeNode tree)
        {
            if (tree == null)
                return -1;
            if (tree.Right == null)
                return tree.Key;
            return Max(tree.Right);
        }
    }

    internal static class SharedA
    {
        public static int Positivise(A a)
        {
            if (a.Field >= a.OtherField)
                return a.Field;
            a.Field++;
            return Positivise(a);
        }

        public static void IncField(A a, int n)
        {
            if (a == null || n <= 0)
                return;
            a.Field++;
            IncField(a, n - 1);
        }

        public static void AtLeastHundred(A a)
        {
            if (a == null)
                return;
            if (a.Field >= 100)
                return;
            a.Field++;
            AtLeastHundred(a);
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
            if (a == null)
                return;
            if (n <= 0)
                return;
            a.Field += a.OtherField;
            AddOther(a, n - 1);
        }

        public static void MoveOtherToField(A a)
        {
            if (a == null)
                return;
            if (a.OtherField <= 0)
                return;
            a.Field++;
            a.OtherField--;
            MoveOtherToField(a);
        }

        public static bool IsFieldGreater(A a)
        {
            if (a == null || a.OtherField < 0)
                return false;
            if (a.OtherField == 0)
                return a.Field > 0;
            a.Field--;
            a.OtherField--;
            return IsFieldGreater(a);
        }

        public static void FibIter(A a, int n)
        {
            if (n <= 0)
                return;
            var tmp = a.Field;
            a.Field += a.OtherField;
            a.OtherField = tmp;
            FibIter(a, n-1);
        }

        public static int AddFields(A a)
        {
            if (a == null || a.Field < 0 || a.OtherField < 0)
                return 0;
            if (a.Field == 0)
                return a.OtherField;
            a.Field--;
            return 1 + AddFields(a);
        }

        public static bool FieldsAreEqual(A a)
        {
            if (a == null || a.Field < 0 || a.OtherField < 0)
                return false;
            if (a.Field == 0)
                return a.OtherField == 0;
            if (a.OtherField == 0)
                return false;
            a.Field--;
            a.OtherField--;
            return FieldsAreEqual(a);
        }
    }

    internal static class SharedList
    {
        public static ListNode RemoveOne(ListNode l, int x)
        {
            if (l == null)
                return null;
            if (l.Key == x)
                return l.Next;
            l.Next = RemoveOne(l.Next, x);
            return l;
        }

        public static ListNode RemoveAll(ListNode l, int x)
        {
            if (l == null)
                return null;
            var tail = RemoveAll(l.Next, x);
            if (l.Key == x)
                return tail;
            l.Next = tail;
            return l;
        }

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

        public static int Mult(int x, int y)
        {
            if (x <= 0)
                return 0;
            return y + Mult(x - 1, y);
        }

        public static ListNode CreateOnes(int n)
        {
            if (n <= 0)
                return null;
            ListNode tail = CreateOnes(n - 1);
            return new ListNode {Key = 1, Next = tail};
        }

        public static bool IsDecreasingFrom(ListNode l, int n)
        {
            if (l == null)
                return true;
            if (l.Key > n)
                return false;
            return IsDecreasingFrom(l.Next, l.Key);
        }

        public static int MaxThan(ListNode l, int max)
        {
            if (l == null)
                return max;
            if (l.Key > max)
                return MaxThan(l.Next, l.Key);
            return MaxThan(l.Next, max);
        }

        public static int Item(ListNode l, int i)
        {
            if (l == null)
                return -1;
            if (i == 0)
                return l.Key;
            return Item(l.Next, i - 1);
        }
    }

    [TestSvmFixture]
    public static class LinearWorkingSafe
    {
        [TestSvm]
        public static void TestMutateRecursive(int n)
        {
            if (n <= 0)
                return;
            var a = new A {Field = 0, OtherField = 0};
            SharedA.IncField(a, n);
            if (a.Field <= 0)
                throw new Exception();
        }

        [TestSvm]
        public static void TestEqualFields(int n, int m)
        {
            var a = new A {Field = n, OtherField = m};
            if (n == m && !SharedA.FieldsAreEqual(a))
                throw new Exception();
        }

        [TestSvm]
        public static void TestFactorialIsGreater(A a)
        {
            if (a == null)
                return;
            var f = a.Field;
            SharedA.Fact(a);
            if (a.OtherField < f)
            {
                throw new Exception();
            }
        }

        [TestSvm]
        public static void TestFieldIsGreater(A a)
        {
            if (a == null || a.OtherField < 0)
                return;
            bool isgr1 = a.Field > a.OtherField;
            bool isgr2 = SharedA.IsFieldGreater(a);
            if (isgr1 != isgr2)
                throw new Exception();
        }

        [TestSvm]
        public static void JustCallTest()
        {
            var a = new A {Field = 42, OtherField = 5};
            SharedA.JustSetField(a);
            if (a.Field != 5)
                throw new Exception();
        }

        [TestSvm]
        public static void CheckMc91Safe(int x)
        {
            if (x <= 96 && McCarthy91.McCarthy(x + 5) != 91)
                throw new Exception();
        }

        [TestSvm]
        public static void TestGcdSame(int n)
        {
            if (GCD.GcdRec(n, n) != n)
                throw new Exception();
        }

        [TestSvm]
        public static void TestGcdEqual(int n, int m)
        {
            if (n == m && GCD.GcdRec(n, m) != n)
                throw new Exception();
        }

        [TestSvm]
        public static void TestGcdIsLess(int n, int m)
        {
            var gcd = GCD.GcdRec(n, m);
            if (!(gcd <= n && gcd <= m))
                throw new Exception();
        }

        [TestSvm]
        public static void TestFieldsEqualRecursive(int x, int y)
        {
            if (x == y && !SharedA.FieldsAreEqual(new A {Field = x, OtherField = y}))
                throw new Exception();
        }
    }

    [TestSvmFixture]
    public static class LinearWorkingUnsafeTest
    {
        [TestSvm]
        public static void TestEqualFields(A a)
        {
            if (a != null && !SharedA.FieldsAreEqual(a))
                throw new Exception();
        }
    }

    [TestSvmFixture]
    public static class LinearWorkingUnsafe
    {
        [TestSvm]
        public static void TestMutateRecursive(int n)
        {
            if (n <= 0)
                return;
            var a = new A {Field = 0, OtherField = 0};
            SharedA.IncField(a, n);
            if (a.Field > 0)
                throw new Exception();
        }

        [TestSvm]
        public static void TestFactorialIsGreater(A a)
        {
            if (a == null)
                return;
            var f = a.Field;
            SharedA.Fact(a);
            if (a.OtherField > f)
            {
                throw new Exception();
            }
        }

        [TestSvm]
        public static void TestFieldIsGreater(A a)
        {
            if (a == null || a.OtherField < 0)
                return;
            bool isgr1 = a.Field == a.OtherField;
            bool isgr2 = SharedA.IsFieldGreater(a);
//            if (isgr1 || isgr2 || true)
//                throw new Exception();
            if (isgr1 || isgr2)
                throw new Exception();
//            if (!isgr2)
//                throw new Exception();
//            if (isgr2)
//                throw new Exception();
//            if (isgr1 && !isgr2 || !isgr1 && isgr2)
//                throw new Exception();
        }

        [TestSvm]
        public static void JustCallTest()
        {
            var a = new A {Field = 42, OtherField = 5};
            SharedA.JustSetField(a);
            if (a.Field == 5)
                throw new Exception();
        }

        [TestSvm]
        public static void CheckMc91Unsafe(int x)
        {
            if (McCarthy91.McCarthy(x + 5) != 91)
            {
                throw new Exception();
            }
        }

        [TestSvm]
        public static void TestGcdSame(int n)
        {
            if (GCD.GcdRec(n, n) > n - 10)
                throw new Exception();
        }

        [TestSvm]
        public static void TestGcdEqual(int n, int m)
        {
            if (n > m && GCD.GcdRec(n, m) != n)
                throw new Exception();
        }

        [TestSvm]
        public static void TestGcdIsLess(int n, int m)
        {
            var gcd = GCD.GcdRec(n, m);
            if (gcd > n - 100 && gcd <= m)
                throw new Exception();
        }

        [TestSvm]
        public static void TestFieldsEqualRecursive(int x, int y)
        {
            if (x == 0 && y == 0 && SharedA.FieldsAreEqual(new A {Field = x, OtherField = 0}))
                throw new Exception();
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
        public static void LastTest(ListNode l)
        {
            if (SharedList.Length(l) == 0 && !(SharedList.Last(l) == -1))
                throw new Exception();
        }

        [TestSvm]
        public static void TestReverseNull(ListNode l)
        {
            if (l != null && SharedList.Reverse(l) == null)
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
        public static void TestLengthOfReverse(ListNode l)
        {
            if (SharedList.Length(l) != SharedList.Length(SharedList.Reverse(l)))
                throw new Exception();
        }

        [Ignore("Mono doesn't work =(")]
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
        public static void ContainsDecreasingOne(int n)
        {
            if (n <= 0)
                return;
            var l = SharedList.CreateDecreasingList(n);
            if (!SharedList.Contains(l, 1))
                throw new Exception();
        }

        [TestSvm]
        public static void ContainsDecreasing(int n, int k)
        {
            if (n <= 0)
                return;
            var l = SharedList.CreateDecreasingList(n);
            if (1 <= k && k <= n && !SharedList.Contains(l, k))
                throw new Exception();
        }

        [TestSvm]
        public static void TestDecreasingIsDecreasing(int n)
        {
            var l = SharedList.CreateDecreasingList(n);
            if (!SharedList.IsDecreasingFrom(l, n))
                throw new Exception();
        }

        [TestSvm]
        public static void TestDecreasingConsIsDecreasing(ListNode l, int n)
        {
            if (SharedList.IsDecreasingFrom(l, n))
            {
                var l2 = new ListNode {Key = n, Next = l};
                if (!SharedList.IsDecreasingFrom(l2, n))
                    throw new Exception();
            }
        }

        [TestSvm]
        public static void TestDecreasingIncIsDecreasing(ListNode l, int n)
        {
            if (!SharedList.IsDecreasingFrom(l, n))
                return;
            SharedList.IncN(l);
            if (!SharedList.IsDecreasingFrom(l, n + 1))
                throw new Exception();
        }

        [TestSvm]
        public static void TestDecreasingLast(ListNode l, int n)
        {
            if (l == null || !SharedList.IsDecreasingFrom(l, n))
                return;
            if (!(SharedList.Last(l) <= l.Key))
                throw new Exception();
        }

        [TestSvm]
        public static void TestContainsReverse(ListNode l, int n)
        {
            if (SharedList.Contains(l, n) && !SharedList.Contains(SharedList.Reverse(l), n))
                throw new Exception();
        }

        [TestSvm]
        public static void TestRemoveOneLength(ListNode l, int n)
        {
            var len1 = SharedList.Length(l);
            var l2 = SharedList.RemoveOne(l, n);
            var len2 = SharedList.Length(l2);
            if (!(len1 == len2 + 1 || len1 == len2))
                throw new Exception();
        }

        [TestSvm]
        public static void TestRemoveAllContains(ListNode l, int x)
        {
            var l2 = SharedList.RemoveAll(l, x);
            if (SharedList.Contains(l, x))
                throw new Exception();
        }

        [TestSvm]
        public static void TestMax(ListNode l)
        {
            if (l == null || !SharedList.IsDecreasingFrom(l, l.Key))
                return;
            if (SharedList.MaxThan(l, l.Key) != l.Key)
                throw new Exception();
        }

        [TestSvm]
        public static void TestItemContains(ListNode l, int i)
        {
            var x = SharedList.Item(l, i);
            if (x != -1 && !SharedList.Contains(l, x))
                throw new Exception();
        }

        [Ignore("Ultimate Timelimit")]
        public static void TestTreeAddContains(BinTreeNode tree, int x)
        {
            if (!SharedTree.Contains(SharedTree.Add(tree, x), x))
                throw new Exception();
        }
    }

    [TestSvmFixture]
    public static class TooHardForSolvers
    {
        [TestSvm]
        public static void TestMutateRecursive(int n)
        {
            if (n <= 0)
                return;
            var a = new A {Field = 0, OtherField = 0};
            SharedA.IncField(a, n);
            if (a.Field != n)
                throw new Exception();
        }

        [TestSvm]
        public static void TestMoveFieldConcrete()
        {
            var a = new A {Field = 0, OtherField = 57};
            var other = a.OtherField;
            SharedA.MoveOtherToField(a);
            if (a.Field != other)
                throw new Exception();
        }

        [TestSvm]
        public static void TestAddFieldsConcrete()
        {
            var a = new A {Field = 42, OtherField = 57};
            int sum1 = a.Field + a.OtherField;
            int sum2 = SharedA.AddFields(a);
            if (sum1 != sum2)
                throw new Exception();
        }

        [TestSvm]
        public static void TestAddFields(A a)
        {
            if (a == null || a.Field < 0 || a.OtherField < 0)
                return;
            int sum1 = a.Field + a.OtherField;
            int sum2 = SharedA.AddFields(a);
            if (sum1 != sum2)
                throw new Exception();
        }

        [TestSvm]
        public static void TestMoveField(A a)
        {
            if (a == null)
                return;
            a.Field = 0;
            var other = a.OtherField;
            SharedA.MoveOtherToField(a);
            if (a.Field != other)
                throw new Exception();
        }

        [TestSvm]
        public static void TestMaxContainsTree(BinTreeNode tree)
        {
            if (tree == null)
                return;
            if (!SharedTree.Contains(tree, SharedTree.Max(tree)))
                throw new Exception();
        }

        [TestSvm]
        public static void AddOtherTest(A a)
        {
            if (a == null)
                return;
            if (a.OtherField > 0 && a.Field == 0)
            {
                SharedA.AddOther(a, 3);
                if (a.Field <= 0)
                    throw new Exception();
            }
        }

        [TestSvm]
        public static void CreateLength(int n)
        {
            if (n >= 0 && SharedList.Length(SharedList.CreateList(n)) != n)
                throw new Exception();
        }

        [TestSvm]
        public static void EqualLength(ListNode l1, ListNode l2)
        {
            if (l1 == l2 && SharedList.Length(l1) != SharedList.Length(l2))
                throw new Exception();
        }

        [TestSvm]
        public static void JustCallTest(A a)
        {
            if (a == null)
                return;
            a.Field = 42;
            a.OtherField = 5;
            SharedA.JustSetField(a);
            if (a.Field != 5)
                throw new Exception();
        }

        [TestSvm]
        public static void JustCallTestSymbolic(A a)
        {
            if (a == null)
                return;
            int x = a.OtherField;
            SharedA.JustSetField(a);
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
                SharedA.StrangeSum(a);
                if (a.Field < 0)
                    throw new Exception();
            }
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
        public static void SumAfterInc(ListNode l)
        {
            var s = SharedList.Sum(l);
            SharedList.IncN(l);
            var s2 = SharedList.Sum(l);
            if (!(s <= s2))
                throw new Exception();
        }

        [TestSvm]
        public static void TestAtLeastHundred(A a)
        {
            if (a == null)
                return;
            SharedA.AtLeastHundred(a);
            if (a.Field < 100)
            {
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
        public static void TestCrop(ListNode l, int n)
        {
            if (n <= 0)
                return;
            SharedList.Crop(l, n);
            if (SharedList.Length(l) > n)
                throw new Exception();
        }

        [TestSvm]
        public static void TestItemLast(ListNode l)
        {
            if (l == null)
                return;
            var last1 = SharedList.Last(l);
            var last2 = SharedList.Item(l, SharedList.Length(l) - 1);
            if (last1 != last2)
                throw new Exception();
        }

        [TestSvm]
        public static void TestSumDecreasing(int n) // nobody supports multiplication
        {
            if (n <= 0)
                return;
            var l = SharedList.CreateDecreasingList(n);
            var s = SharedList.Sum(l);
            if (s + s != SharedList.Mult(n, n + 1))
                throw new Exception();
        }

        [TestSvm]
        public static void TestLastInc(ListNode l)
        {
            if (l == null)
                return;
            var x = SharedList.Last(l);
            SharedList.IncN(l);
            if (SharedList.Last(l) != x + 1)
                throw new Exception();
        }

        [TestSvm]
        public static void TestLastOnes(int n)
        {
            if (n <= 0)
                return;
            var l = SharedList.CreateOnes(n);
            if (SharedList.Last(l) != 1)
                throw new Exception();
        }

        [TestSvm]
        public static void TestSumOnes(int n)
        {
            if (n <= 0)
                return;
            var l = SharedList.CreateOnes(n);
            var s = SharedList.Sum(l);
            if (s != n)
                throw new Exception();
        }
    }

    [TestSvmFixture]
    public static class WIP
    {
        [Ignore("Error came to Encode")]
        public static void TestPositiveRecursion(int n)
        {
            var a = new A {Field = 0, OtherField = n};
            if (SharedA.Positivise(a) < 0)
                throw new Exception();
        }

        [Ignore("Mono error")]
        public static void StrangeSumTestConcrete()
        {
            var a = new A {Field = 0, OtherField = 30};
            SharedA.StrangeSum(a);
            if (a.Field < 0)
                throw new Exception();
        }

        [Ignore("Internal error: stack does not contain key (num, LocalVariable:-1283898937)!")]
        public static void TestFibA(int n)
        {
            if (n <= 0)
                return;
            var a = new A {Field = 1, OtherField = 1};
            SharedA.FibIter(a, n);
            if (!(a.Field >= n))
                throw new Exception();
        }

        [Ignore("Internal error: stack does not contain key (tree, LocalVariable:-1283898937)!")]
        public static void TestFromListContains(ListNode list)
        {
            if (list == null)
                return;
            var tree = SharedTree.FromList(list);
            if (!SharedTree.Contains(tree, list.Key))
                throw new Exception();
        }

        [Ignore("TODO: wrong ML generated")]
        public static void TestFromListNotNull(ListNode list)
        {
            if (list != null && SharedTree.FromList(list) == null)
                throw new Exception();
        }

        [Ignore("Internal error: stack does not contain key (this, 600024B)!")]
        public static void TestBinTree(BinTree tree, int x)
        {
            if (tree == null)
                return;
            tree.Add(x);
            if (!tree.Contains(x))
                throw new Exception();
        }

        [Ignore("Too many clauses (cartesianMap) in Encode.Relations")]
        public static void TestRemoveAllDouble(ListNode l, int x)
        {
            var l1 = SharedList.RemoveAll(l, x);
            var len1 = SharedList.Length(l1);
            var l2 = SharedList.RemoveAll(l1, x);
            var len2 = SharedList.Length(l2);
            if (len1 != len2)
                throw new Exception();
        }

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
