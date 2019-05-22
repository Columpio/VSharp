﻿using System;
using System.IO;
using System.Reflection;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using NUnit.Framework;
using NUnit.Framework.Interfaces;
using NUnit.Framework.Internal;
using NUnit.Framework.Internal.Builders;
using NUnit.Framework.Internal.Commands;
using VSharp.Core;
using VSharp.Interpreter;

namespace VSharp.Test
{
    public class DumpStackTraceListener : TraceListener
    {
        public override void Write(string message)
        {
            Console.Write(message);
        }

        public override void WriteLine(string message)
        {
            Console.WriteLine(message);
        }

        public override void Fail(string message)
        {
            Fail(message, string.Empty);
        }

        public override void Fail(string message1, string message2)
        {
            Console.WriteLine("ASSERT FAILED");
            Console.WriteLine("{0}: {1}", message1, message2);
            Console.WriteLine("Stack Trace:");

            StackTrace trace = new StackTrace( true );
            StackFrame[] stackFrames = trace.GetFrames();
            if (stackFrames != null)
            {
                foreach (StackFrame frame in stackFrames)
                {
                    MethodBase frameClass = frame.GetMethod();
                    Console.WriteLine("  {2}.{3} {0}:{1}",
                        frame.GetFileName(),
                        frame.GetFileLineNumber(),
                        frameClass.DeclaringType,
                        frameClass.Name);
                }
            }
        }
    }

    [SetUpFixture]
    public class SetUpSvm
    {
        [OneTimeSetUp]
        public void PrepareSvm()
        {
            Trace.Listeners.Add(new DumpStackTraceListener());

            var ci = new CultureInfo("en-GB")
            {
                NumberFormat = {
                    PositiveInfinitySymbol = "Infinity",
                    NegativeInfinitySymbol = "-Infinity"
                }
            };
            Thread.CurrentThread.CurrentCulture = ci;

            // Something like Propositional.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)
            SVM.ConfigureSolver(new SmtSolverWrapper<Microsoft.Z3.AST>(new Z3Solver()));
        }
    }

    public class TestSvmFixtureAttribute : NUnitAttribute, IFixtureBuilder
    {
        private class DummyFilter : IPreFilter
        {
            public bool IsMatch(Type type)
            {
                return true;
            }

            public bool IsMatch(Type type, MethodInfo method)
            {
                return true;
            }
        }

        private class DummyTypeInfo : ITypeInfo
        {
            /*
             * This class is mostly a hack to bypass NUnit test-class validation checks
             * See: NUnit.Framework.Internal.Builders.NUnitTestFixtureBuilder.CheckTestFixtureIsValid code
             */
            public Type Type { get; }

            public DummyTypeInfo(Type type)
            {
                Type = type;
            }

            public ITypeInfo BaseType
            {
                get
                {
                    var baseType = Type.GetTypeInfo().BaseType;

                    return baseType != null
                        ? new TypeWrapper(baseType)
                        : null;
                }
            }

            public string Name => Type.Name;

            public string FullName => Type.FullName;

            public Assembly Assembly => Type.GetTypeInfo().Assembly;

            public string Namespace => Type.Namespace;

            public bool IsAbstract => Type.GetTypeInfo().IsAbstract;

            public bool IsGenericType => false;

            public bool IsType(Type type)
            {
                return Type == type;
            }

            public bool ContainsGenericParameters => false;

            public bool IsGenericTypeDefinition => false;

            public bool IsSealed => Type.GetTypeInfo().IsSealed;

            public bool IsStaticClass => true; // Type.IsAbstract && Type.IsSealed;

            public string GetDisplayName()
            {
                return TypeHelper.GetDisplayName(Type);
            }

            public string GetDisplayName(object[] args)
            {
                return TypeHelper.GetDisplayName(Type, args);
            }

            public ITypeInfo MakeGenericType(Type[] typeArgs)
            {
                return new TypeWrapper(Type.MakeGenericType(typeArgs));
            }

            public Type GetGenericTypeDefinition()
            {
                return Type.GetGenericTypeDefinition();
            }

            public T[] GetCustomAttributes<T>(bool inherit) where T : class
            {
                return (T[])((ICustomAttributeProvider)Type.GetTypeInfo()).GetCustomAttributes(typeof(T), inherit);
            }

            public bool IsDefined<T>(bool inherit) where T : class
            {
                return ((ICustomAttributeProvider) Type.GetTypeInfo()).IsDefined(typeof(T), inherit);
            }

            public bool HasMethodWithAttribute(Type attributeType)
            {
                return Reflect.HasMethodWithAttribute(Type, attributeType);
            }

            public IMethodInfo[] GetMethods(BindingFlags flags)
            {
                var methods = Type.GetMethods(flags);
                var result = new MethodWrapper[methods.Length];

                for (int i = 0; i < methods.Length; i++)
                    result[i] = new MethodWrapper(Type, methods[i]);

                return result;
            }

            public ConstructorInfo GetConstructor(Type[] argTypes)
            {
                return Type.GetConstructor(argTypes);
            }

            public bool HasConstructor(Type[] argTypes)
            {
                return GetConstructor(argTypes) != null;
            }

            public object Construct(object[] args)
            {
                return null; // Reflect.Construct(Type, args);
            }

            public override string ToString()
            {
                return Type.ToString();
            }
        }

        public IEnumerable<TestSuite> BuildFrom(ITypeInfo typeInfo)
        {
            var typ = new DummyTypeInfo(typeInfo.Type);
            yield return new NUnitTestFixtureBuilder().BuildFrom(typ, new DummyFilter());
        }
    }

    public class IdealValuesHandler
    {
        private const string MethodSeparator = "METHOD: ";
        private const string ResultSeparator = "RESULT: ";
        private const string GoldsDirectoryName = "Golds";
        private const string IdealTestFileExtension = ".gold";
//        private const string IdealTemporaryFileExtension = ".tmp";
        private const string IdealTemporaryFileExtension = "";

        private string _idealValuePath;
        public string ExpectedValue;
        private string _methodName;

        public IdealValuesHandler(MethodInfo methodInfo, [CallerFilePath] string currentFilePath = "")
        {
            var currentFolder = Path.GetDirectoryName(currentFilePath);
            _idealValuePath = GetIdealValuePath(currentFolder, methodInfo);
            ExpectedValue = ReadIdealValue(_idealValuePath);
            _methodName = MethodInfoToString(methodInfo);
            Options.CurrentMethodName = Path.Combine(Path.GetDirectoryName(_idealValuePath),
                Path.GetFileNameWithoutExtension(_idealValuePath));
        }

        private static string GetIdealValuePath(string currentFolder, MethodInfo methodInfo)
        {
            var typeName = methodInfo?.DeclaringType?.FullName?.Split('.');
            if (typeName == null)
                return null;
            var os = Environment.OSVersion.Platform.ToString();
            var paramsHash = methodInfo.GetParameters().Select(p => p.ToString().GetHashCode()).ToArray();
            var hash = paramsHash.Aggregate(paramsHash.Length, (current, t) => unchecked(current * 314159 + t));
            var methodName = $"{methodInfo.Name}.{os}.{hash}{IdealTestFileExtension}";
            var idealValuePath = Path.Combine(currentFolder, GoldsDirectoryName, Path.Combine(typeName), methodName);
            return idealValuePath;
        }

        private static string ReadIdealValue(string idealValuePath)
        {
            if (!File.Exists(idealValuePath))
                return null;
            var idealValueContents = File.ReadAllText(idealValuePath);
            if (string.IsNullOrEmpty(idealValueContents))
                return null;

            var idealValue = idealValueContents.Split(new []{ResultSeparator}, 2, StringSplitOptions.None);
            return idealValue[1].Trim();
        }

        private static string MethodInfoToString(MethodInfo methodInfo)
        {
            var parameters = string.Join(", ", methodInfo.GetParameters().Select(p => p.ParameterType.ToString()));
            return $"{methodInfo.ReturnType} {methodInfo.DeclaringType}.{methodInfo.Name}({parameters})";
        }

        public void CreateTemporaryIdealFile(string gotValue)
        {
            var text = $"{MethodSeparator}{_methodName}\n{ResultSeparator}{gotValue}\n";
            var idealValueRoot = Path.GetDirectoryName(_idealValuePath);
            Debug.Assert(idealValueRoot != null);
            Directory.CreateDirectory(idealValueRoot);
            File.WriteAllText(_idealValuePath + IdealTemporaryFileExtension, text);
        }

        public string DiffOfGotAndIdealValues(string gotValue)
        {
            return ExpectedValue == null
                ? $"There is no gold file for {_methodName}!\nGOT: {gotValue}"
                : $"{MethodSeparator}{_methodName}\nEXPECTED: {ExpectedValue}\nGOT:      {gotValue}";
        }
    }

    public class TestSvmAttribute : NUnitAttribute, IWrapTestMethod, ISimpleTestBuilder
    {
        public TestCommand Wrap(TestCommand command)
        {
            return new TestSvmCommand(command);
        }

        private class TestSvmCommand : DelegatingTestCommand
        {
            public TestSvmCommand(TestCommand innerCommand) : base(innerCommand) {}

            public override TestResult Execute(TestExecutionContext context)
            {
                var methodInfo = innerCommand.Test.Method.MethodInfo;
                var idealValue = new IdealValuesHandler(methodInfo);
                var gotValue = SVM.ExploreOne(methodInfo);

                if (string.Equals(idealValue.ExpectedValue, gotValue))
                {
                    context.CurrentResult.SetResult(ResultState.Success);
                }
                else
                {
                    idealValue.CreateTemporaryIdealFile(gotValue);
                    var diff = idealValue.DiffOfGotAndIdealValues(gotValue);
                    context.CurrentResult.SetResult(ResultState.Failure, diff);
                }
                return context.CurrentResult;
            }
        }

        private static NUnitTestCaseBuilder _builder = new NUnitTestCaseBuilder();

        public TestMethod BuildFrom(IMethodInfo method, NUnit.Framework.Internal.Test suite)
        {
            var defaultParameters = method.GetParameters().Select(
                parameter => TypeUtils.defaultOf(parameter.ParameterType)).ToArray();
            var parameters = new TestCaseParameters(defaultParameters);
            if (method.ReturnType.Type != typeof(void))
                parameters.ExpectedResult = null;
            return _builder.BuildTestMethod(method, suite, parameters);
        }
    }
}
