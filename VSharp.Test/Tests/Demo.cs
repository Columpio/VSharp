using System;
using System.Collections;
using System.Collections.Generic;
using NUnit.Framework;
using VSharp.Test;

namespace VSharp.Test.Tests.Demo
{
    public class Tree<T> : ICollection<T>, IList<T> where T : IComparable<T>
    {
        public class TreeNode : ICollection<T>, IList<T>
        {
            public TreeNode(T value, Tree<T> tree)
            {
                Value = value;
                Level = 1;
                Count = 1;
                Tree = tree;
            }

            public Tree<T> Tree { get; private set; }
            public T Value { get; private set; }
            public TreeNode Parent { get; private set; }
            public TreeNode LeftHand { get; private set; }
            public TreeNode RightHand { get; private set; }
            int Level { get; set; }
            public int Count { get; private set; }

            public void Add(T item)
            {
                var compare = item.CompareTo(Value);
                if (compare < 0)
                    if (LeftHand == null)
                        ((LeftHand = new TreeNode(item, Tree)).Parent = this).Reconstruct(true);
                    else LeftHand.Add(item);
                else
                    if (RightHand == null)
                        ((RightHand = new TreeNode(item, Tree)).Parent = this).Reconstruct(true);
                    else RightHand.Add(item);
            }

            public void Clear()
            {
                if (LeftHand != null) LeftHand.Clear();
                if (RightHand != null) RightHand.Clear();
                LeftHand = RightHand = null;
            }

            public bool Contains(T item)
            {
                var compare = item.CompareTo(Value);
                if (compare < 0)
                    return LeftHand == null ? false : LeftHand.Contains(item);
                else if (compare == 0)
                    return true;
                else
                    return RightHand == null ? false : RightHand.Contains(item);
            }

            public void CopyTo(T[] array, int arrayIndex)
            {
                if (LeftHand != null)
                {
                    LeftHand.CopyTo(array, arrayIndex);
                    arrayIndex += LeftHand.Count;
                }
                array[arrayIndex++] = Value;
                if (RightHand != null)
                    RightHand.CopyTo(array, arrayIndex);
            }

            public bool IsReadOnly { get { return false; } }

            public bool Remove(T item)
            {
                var compare = item.CompareTo(Value);
                if (compare == 0)
                {
                    if (LeftHand == null && RightHand == null)
                        if (Parent != null)
                        {
                            if (Parent.LeftHand == this) Parent.LeftHand = null;
                            else Parent.RightHand = null;
                            Parent.Reconstruct(true);
                        }
                        else Tree.RootNode = null;
                    else if (LeftHand == null || RightHand == null)
                    {
                        var child = LeftHand == null ? RightHand : LeftHand;
                        if (Parent != null)
                        {
                            if (Parent.LeftHand == this) Parent.LeftHand = child;
                            else Parent.RightHand = child;
                            (child.Parent = Parent).Reconstruct(true);
                        }
                        else (Tree.RootNode = child).Parent = null;
                    }
                    else
                    {
                        var replace = LeftHand;
                        while (replace.RightHand != null) replace = replace.RightHand;
                        var temp = Value;
                        Value = replace.Value;
                        replace.Value = temp;
                        return replace.Remove(replace.Value);
                    }
                    Parent = LeftHand = RightHand = null;
                    return true;
                }
                else if (compare < 0)
                    return LeftHand == null ? false : LeftHand.Remove(item);
                else
                    return RightHand == null ? false : RightHand.Remove(item);
            }

            public IEnumerator<T> GetEnumerator()
            {
/*                if (LeftHand != null)
                    foreach (var item in LeftHand)
                        yield return item;
                yield return Value;
                if (RightHand != null)
                    foreach (var item in RightHand)
                        yield return item;*/
                throw new NotImplementedException();
            }

            IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }

            void Reconstruct(bool recursive)
            {
                Count = 1;

                int leftLevel = 0, rightLevel = 0;
                if (LeftHand != null)
                {
                    leftLevel = LeftHand.Level;
                    Count += LeftHand.Count;
                }
                if (RightHand != null)
                {
                    rightLevel = RightHand.Level;
                    Count += RightHand.Count;
                }

                if (leftLevel - rightLevel > 1)
                {
                    var leftLeft = LeftHand.LeftHand == null ? 0 : LeftHand.LeftHand.Level;
                    var leftRight = LeftHand.RightHand == null ? 0 : LeftHand.RightHand.Level;
                    if (leftLeft >= leftRight)
                    {
                        LeftHand.Elevate();
                        Reconstruct(true);
                    }
                    else
                    {
                        var pivot = LeftHand.RightHand;
                        pivot.Elevate(); pivot.Elevate();
                        pivot.LeftHand.Reconstruct(false);
                        pivot.RightHand.Reconstruct(true);
                    }
                }
                else if (rightLevel - leftLevel > 1)
                {
                    var rightRight = RightHand.RightHand == null ? 0 : RightHand.RightHand.Level;
                    var rightLeft = RightHand.LeftHand == null ? 0 : RightHand.LeftHand.Level;
                    if (rightRight >= rightLeft)
                    {
                        RightHand.Elevate();
                        Reconstruct(true);
                    }
                    else
                    {
                        var pivot = RightHand.LeftHand;
                        pivot.Elevate(); pivot.Elevate();
                        pivot.LeftHand.Reconstruct(false);
                        pivot.RightHand.Reconstruct(true);
                    }
                }
                else
                {
                    Level = Math.Max(leftLevel, rightLevel) + 1;
                    if (Parent != null && recursive)
                        Parent.Reconstruct(true);
                }
            }

            void Elevate()
            {
                var root = Parent;
                var parent = root.Parent;
                if ((Parent = parent) == null) Tree.RootNode = this;
                else
                {
                    if (parent.LeftHand == root) parent.LeftHand = this;
                    else parent.RightHand = this;
                }

                if (root.LeftHand == this)
                {
                    root.LeftHand = RightHand;
                    if (RightHand != null) RightHand.Parent = root;
                    RightHand = root;
                    root.Parent = this;
                }
                else
                {
                    root.RightHand = LeftHand;
                    if (LeftHand != null) LeftHand.Parent = root;
                    LeftHand = root;
                    root.Parent = this;
                }
            }

            public int IndexOf(T item)
            {
                var compare = item.CompareTo(Value);
                if (compare == 0)
                    if (LeftHand == null) return 0;
                    else
                    {
                        var temp = LeftHand.IndexOf(item);
                        return temp == -1 ? LeftHand.Count : temp;
                    }
                else if (compare < 0)
                    if (LeftHand == null) return -1;
                    else return LeftHand.IndexOf(item);
                else
                    if (RightHand == null) return -1;
                    else return RightHand.IndexOf(item);
            }

            public void Insert(int index, T item) { throw new InvalidOperationException(); }

            public void RemoveAt(int index)
            {
                if (LeftHand != null)
                    if (index < LeftHand.Count)
                    {
                        LeftHand.RemoveAt(index);
                        return;
                    }
                    else index -= LeftHand.Count;
                if (index-- == 0)
                {
                    Remove(Value);
                    return;
                }
                if (RightHand != null)
                    if (index < RightHand.Count)
                    {
                        RightHand.RemoveAt(index);
                        return;
                    }
                throw new ArgumentOutOfRangeException("index");
            }

            public T this[int index]
            {
                get
                {
                    if (LeftHand != null)
                        if (index < LeftHand.Count) return LeftHand[index];
                        else index -= LeftHand.Count;
                    if (index-- == 0) return Value;
                    if (RightHand != null)
                        if (index < RightHand.Count) return RightHand[index];
                    throw new ArgumentOutOfRangeException("index");
                }
                set { throw new InvalidOperationException(); }
            }
        }

        public TreeNode RootNode { get; private set; }

        public void Add(T item)
        {
            if (RootNode == null) RootNode = new TreeNode(item, this);
            else RootNode.Add(item);
        }

        public void Clear()
        {
            if (RootNode == null) return;
            RootNode.Clear();
            RootNode = null;
        }

        public bool Contains(T item) { return RootNode == null ? false : RootNode.Contains(item); }

        public void CopyTo(T[] array, int arrayIndex)
        {
            if (array == null) throw new ArgumentNullException("array");
            if (arrayIndex < 0) throw new ArgumentOutOfRangeException("arrayIndex");
            if ((array.Length <= arrayIndex) || (RootNode != null && array.Length < arrayIndex + RootNode.Count))
                throw new ArgumentException();

            if (RootNode != null)
                RootNode.CopyTo(array, arrayIndex);
        }

        public int Count { get { return RootNode.Count; } }

        public bool IsReadOnly { get { return false; } }

        public bool Remove(T item) { return RootNode == null ? false : RootNode.Remove(item); }

        public IEnumerator<T> GetEnumerator()
        {
/*            if (RootNode != null)
                foreach (var item in RootNode)
                    yield return item;
            else
                yield break;*/
            throw new NotImplementedException();
        }

        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }

        public int IndexOf(T item) { return RootNode != null ? RootNode.IndexOf(item) : -1; }

        public void Insert(int index, T item) { throw new InvalidOperationException(); }

        public void RemoveAt(int index) { if (RootNode != null) RootNode.RemoveAt(index); }

        public T this[int index]
        {
            get
            {
                if (RootNode != null) return RootNode[index];
                else throw new ArgumentOutOfRangeException("index");
            }
            set { throw new InvalidOperationException(); }
        }
    }

    public sealed class IntTree : ICollection<int>, IList<int>
    {
        public sealed class TreeNode : ICollection<int>, IList<int>
        {
            public TreeNode(int value, IntTree tree)
            {
                Value = value;
                Level = 1;
                Count = 1;
                Tree = tree;
            }

            public IntTree Tree { get; set; }
            public int Value { get; set; }
            public TreeNode Parent { get; set; }
            public TreeNode LeftHand { get; set; }
            public TreeNode RightHand { get; set; }
            int Level { get; set; }
            public int Count { get; private set; }

            public void Add(int item)
            {
                var compare = item.CompareTo(Value);
                if (compare < 0)
                    if (LeftHand == null)
                        ((LeftHand = new TreeNode(item, Tree)).Parent = this).Reconstruct(true);
                    else LeftHand.Add(item);
                else
                    if (RightHand == null)
                        ((RightHand = new TreeNode(item, Tree)).Parent = this).Reconstruct(true);
                    else RightHand.Add(item);
            }

            public void Clear()
            {
                if (LeftHand != null) LeftHand.Clear();
                if (RightHand != null) RightHand.Clear();
                LeftHand = RightHand = null;
            }

            public bool Contains(int item)
            {
                if (item < Value)
                    return LeftHand == null ? false : LeftHand.Contains(item);
                if (item == Value)
                    return true;
                return RightHand == null ? false : RightHand.Contains(item);
            }

            public void CopyTo(int[] array, int arrayIndex)
            {
                if (LeftHand != null)
                {
                    LeftHand.CopyTo(array, arrayIndex);
                    arrayIndex += LeftHand.Count;
                }
                array[arrayIndex++] = Value;
                if (RightHand != null)
                    RightHand.CopyTo(array, arrayIndex);
            }

            public void NoErrorIfLeftsDontFormACycle()
            {
                LeftHand.NoErrorIfLeftsDontFormACycle();
            }

            public void JustGoLeft()
            {
                var leftHand = LeftHand;
                if (leftHand == null)
                    return;
                leftHand.JustGoLeft();
            }

            public bool IsReadOnly { get { return false; } }

            public bool Remove(int item)
            {
                var compare = item.CompareTo(Value);
                if (compare == 0)
                {
                    if (LeftHand == null && RightHand == null)
                        if (Parent != null)
                        {
                            if (Parent.LeftHand == this) Parent.LeftHand = null;
                            else Parent.RightHand = null;
                            Parent.Reconstruct(true);
                        }
                        else Tree.RootNode = null;
                    else if (LeftHand == null || RightHand == null)
                    {
                        var child = LeftHand == null ? RightHand : LeftHand;
                        if (Parent != null)
                        {
                            if (Parent.LeftHand == this) Parent.LeftHand = child;
                            else Parent.RightHand = child;
                            (child.Parent = Parent).Reconstruct(true);
                        }
                        else (Tree.RootNode = child).Parent = null;
                    }
                    else
                    {
                        var replace = LeftHand;
                        while (replace.RightHand != null) replace = replace.RightHand;
                        var temp = Value;
                        Value = replace.Value;
                        replace.Value = temp;
                        return replace.Remove(replace.Value);
                    }
                    Parent = LeftHand = RightHand = null;
                    return true;
                }
                else if (compare < 0)
                    return LeftHand == null ? false : LeftHand.Remove(item);
                else
                    return RightHand == null ? false : RightHand.Remove(item);
            }

            public IEnumerator<int> GetEnumerator()
            {
/*                if (LeftHand != null)
                    foreach (var item in LeftHand)
                        yield return item;
                yield return Value;
                if (RightHand != null)
                    foreach (var item in RightHand)
                        yield return item;*/
                throw new NotImplementedException();
            }

            IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }

            void Reconstruct(bool recursive)
            {
                Count = 1;

                int leftLevel = 0, rightLevel = 0;
                if (LeftHand != null)
                {
                    leftLevel = LeftHand.Level;
                    Count += LeftHand.Count;
                }
                if (RightHand != null)
                {
                    rightLevel = RightHand.Level;
                    Count += RightHand.Count;
                }

                if (leftLevel - rightLevel > 1)
                {
                    var leftLeft = LeftHand.LeftHand == null ? 0 : LeftHand.LeftHand.Level;
                    var leftRight = LeftHand.RightHand == null ? 0 : LeftHand.RightHand.Level;
                    if (leftLeft >= leftRight)
                    {
                        LeftHand.Elevate();
                        Reconstruct(true);
                    }
                    else
                    {
                        var pivot = LeftHand.RightHand;
                        pivot.Elevate(); pivot.Elevate();
                        pivot.LeftHand.Reconstruct(false);
                        pivot.RightHand.Reconstruct(true);
                    }
                }
                else if (rightLevel - leftLevel > 1)
                {
                    var rightRight = RightHand.RightHand == null ? 0 : RightHand.RightHand.Level;
                    var rightLeft = RightHand.LeftHand == null ? 0 : RightHand.LeftHand.Level;
                    if (rightRight >= rightLeft)
                    {
                        RightHand.Elevate();
                        Reconstruct(true);
                    }
                    else
                    {
                        var pivot = RightHand.LeftHand;
                        pivot.Elevate(); pivot.Elevate();
                        pivot.LeftHand.Reconstruct(false);
                        pivot.RightHand.Reconstruct(true);
                    }
                }
                else
                {
                    Level = Math.Max(leftLevel, rightLevel) + 1;
                    if (Parent != null && recursive)
                        Parent.Reconstruct(true);
                }
            }

            public void Elevate()
            {
                var root = Parent;
                var parent = root.Parent;
                if ((Parent = parent) == null) Tree.RootNode = this;
                else
                {
                    if (parent.LeftHand == root) parent.LeftHand = this;
                    else parent.RightHand = this;
                }

                if (root.LeftHand == this)
                {
                    root.LeftHand = RightHand;
                    if (RightHand != null) RightHand.Parent = root;
                    RightHand = root;
                    root.Parent = this;
                }
                else
                {
                    root.RightHand = LeftHand;
                    if (LeftHand != null) LeftHand.Parent = root;
                    LeftHand = root;
                    root.Parent = this;
                }
            }

            public int IndexOf(int item)
            {
                var compare = item.CompareTo(Value);
                if (compare == 0)
                    if (LeftHand == null) return 0;
                    else
                    {
                        var temp = LeftHand.IndexOf(item);
                        return temp == -1 ? LeftHand.Count : temp;
                    }
                else if (compare < 0)
                    if (LeftHand == null) return -1;
                    else return LeftHand.IndexOf(item);
                else
                    if (RightHand == null) return -1;
                    else return RightHand.IndexOf(item);
            }

            public void Insert(int index, int item) { throw new InvalidOperationException(); }

            public void RemoveAt(int index)
            {
                if (LeftHand != null)
                    if (index < LeftHand.Count)
                    {
                        LeftHand.RemoveAt(index);
                        return;
                    }
                    else index -= LeftHand.Count;
                if (index-- == 0)
                {
                    Remove(Value);
                    return;
                }
                if (RightHand != null)
                    if (index < RightHand.Count)
                    {
                        RightHand.RemoveAt(index);
                        return;
                    }
                throw new ArgumentOutOfRangeException("index");
            }

            public int this[int index]
            {
                get
                {
                    if (LeftHand != null)
                        if (index < LeftHand.Count) return LeftHand[index];
                        else index -= LeftHand.Count;
                    if (index-- == 0) return Value;
                    if (RightHand != null)
                        if (index < RightHand.Count) return RightHand[index];
                    throw new ArgumentOutOfRangeException("index");
                }
                set { throw new InvalidOperationException(); }
            }
        }

        public TreeNode RootNode { get; private set; }

        public void Add(int item)
        {
            if (RootNode == null) RootNode = new TreeNode(item, this);
            else RootNode.Add(item);
        }

        public void Clear()
        {
            if (RootNode == null) return;
            RootNode.Clear();
            RootNode = null;
        }

        public bool Contains(int item) { return RootNode == null ? false : RootNode.Contains(item); }

        public void CopyTo(int[] array, int arrayIndex)
        {
            if (array == null) throw new ArgumentNullException("array");
            if (arrayIndex < 0) throw new ArgumentOutOfRangeException("arrayIndex");
            if ((array.Length <= arrayIndex) || (RootNode != null && array.Length < arrayIndex + RootNode.Count))
                throw new ArgumentException();

            if (RootNode != null)
                RootNode.CopyTo(array, arrayIndex);
        }

        public int Count { get { return RootNode.Count; } }

        public bool IsReadOnly { get { return false; } }

        public bool Remove(int item) { return RootNode == null ? false : RootNode.Remove(item); }

        public IEnumerator<int> GetEnumerator()
        {
/*            if (RootNode != null)
                foreach (var item in RootNode)
                    yield return item;
            else
                yield break;*/
            throw new NotImplementedException();
        }

        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }

        public int IndexOf(int item) { return RootNode != null ? RootNode.IndexOf(item) : -1; }

        public void Insert(int index, int item) { throw new InvalidOperationException(); }

        public void RemoveAt(int index) { if (RootNode != null) RootNode.RemoveAt(index); }

        public int this[int index]
        {
            get
            {
                if (RootNode != null) return RootNode[index];
                else throw new ArgumentOutOfRangeException("index");
            }
            set { throw new InvalidOperationException(); }
        }
    }

    [TestSvmFixture]
    public static class WorkingTests
    {
        // TODO: these two tests need new result constructor: ThrowOrVoid, Composition of results, ...
        static void CallNoErrorIfLeftsDontFormACycle(IntTree avl)
        {
            avl.RootNode.NoErrorIfLeftsDontFormACycle(); // Error if left has no cycles, else no termination
        }

        static void CallGoLeft(IntTree avl)
        {
            avl.RootNode.JustGoLeft(); // always good
        }

        private static void RecInc(int n, IntTree.TreeNode avl)
        {
            if (n <= 0)
                return;
            avl.Value = n;
            RecInc(n - 1, avl);
        }

        public static void RecCall(int n, IntTree avl)
        {
            if (avl == null)
                return;
            RecInc(n, avl.RootNode);
            if (n > 0 && avl.RootNode.Value != 1)
                throw new Exception();
        }
    }

    static class BugFixesTests
    {
        private static void RecInc(int n, IntTree.TreeNode avl)
        {}
        private static void RecInc2(int n, IntTree.TreeNode avl)
        {
            avl = avl.LeftHand;
        }

        public static void RecCall1()
        {
            IntTree avl = null;
            RecInc(0, avl.RootNode);
        }

        public static void RecCall2(IntTree avl)
        {
            RecInc(0, avl.RootNode);
        }

        public static void RecCall3(IntTree avl)
        {
            RecInc2(0, avl.RootNode);
        }
    }

    [TestSvmFixture]
    public static class IntTests
    {
        [Ignore("Internal error: stack does not contain key (dimension, MethodParameter:774163403)")]
        public static void FullySymbolicCopy(IntTree avl, int[] arr)
        {
            avl.CopyTo(arr, 0);
        }

        [Ignore("Internal error: stack does not contain key (item, MethodParameter:-247148947)")]
        public static bool CallMethods(IntTree avl)
        {
            return avl.Contains(42);
        }

        [Ignore("HeapRef(Union ...)")]
        public static bool RemoveTest(IntTree avl, int x)
        {
            return avl.Remove(x);
        }

        [Ignore("Internal error: stack does not contain key (index, MethodParameter:1839632078)!")]
        public static int SimpleContains(IntTree avl, int x)
        {
            return avl[x];
        }

        [Ignore("HeapRef(Union ...)")]
        public static bool RemoveAfterAdd(IntTree avl, int x)
        {
            avl.Add(x);
            return true; //avl.Remove(x); // true
        }
    }

    static class Tests
    {
/*
        public static void RemoveAll<T>(Tree<T> avl, T x) where T : IComparable<T>
        {
            while (avl.Contains(x))
            {
                avl.Remove(x);
            }
        }*/
/*
        public static bool ContainsAfterRemove<T>(Tree<T> avl, T x) where T : IComparable<T>
        {
            RemoveAll(avl, x);

            return avl.Contains(x); // false
        }*/

/*        // TODO: stack overflow
        public static void FullySymbolicCopy<T>(Tree<T> avl, T[] arr) where T : IComparable<T>
        {
            avl.CopyTo(arr, 0);
        }*/

        /* [works] if arr.Length == 0 then IndexOutOfBound else do nothing */
/*        public static void IdentityForOneElementArrays<T>(T[] arr) where T : IComparable<T>
        {
            var avl = new Tree<T>();
            avl.Add(arr[0]);
            avl.CopyTo(arr, 0);
        }*/

/*        public static void SortArray<T>(T[] arr) where T : IComparable<T> // TODO: needs delegates
        {
            var avl = new Tree<T>();
            for (int i = 0; i < arr.Length; i++)
                avl.Add(arr[i]);
            avl.CopyTo(arr, 0);
        }*/

/*        public static void SortArrayTest()
        {
            var t = new [] { 42, 1, 2, 58, 3, 15 };
            foreach (var x in t)
            {
                Console.WriteLine(x);
            }

            Console.WriteLine("Sorting...");
            SortArray(t);


            foreach (var x in t)
            {
                Console.WriteLine(x);
            }
        }*/
    }
}
