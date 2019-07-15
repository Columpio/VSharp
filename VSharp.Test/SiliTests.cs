using System;
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
    internal class DumpStackTraceListener : TraceListener
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
            Fail(message, String.Empty);
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
    internal class SetUpSvm
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

    internal class TestSvmFixtureAttribute : NUnitAttribute, IFixtureBuilder
    {
        private class DummyFilter : IPreFilter
        {
            /* Filter for exploring all possible methods */
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
             * (NUnit doesn't allow test class to be generic with not specified parameters
             * However, we want to keep generic classes generic, e.g. in ``Tests/Generic.cs")
             * It's a copy-paste of NUnit.Framework.Internal.TypeWrapper with certain modifications
             * (e.g. ``ContainsGenericParameters" always returns ``false")
             * For NUnit validation checks see:
             * NUnit.Framework.Internal.Builders.NUnitTestFixtureBuilder.CheckTestFixtureIsValid
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

            public bool IsStaticClass => true;

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
                return null;
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

    internal class IdealValuesHandler
    {
        private const string MethodSeparator = "METHOD: ";
        private const string ResultSeparator = "RESULT: ";
        private const string GoldsDirectoryName = "Golds";
        private static string _osGoldDirectoryName = Environment.OSVersion.Platform.ToString();
        internal static string IdealUnrollingOptionDirectoryName = GetIdealUnrollingOptionDirectoryName();
        private const string IdealTestFileExtension = ".gold";
        private const string IdealTemporaryFileExtension = ".tmp";

        private string _idealValuePath;
        public string ExpectedValue;
        private string _methodName;

        private static string GetIdealUnrollingOptionDirectoryName()
        {
            var mode = Options.RecursionUnrollingMode();
            if (mode.Equals(RecursionUnrollingModeType.SmartUnrolling))
                return "SmartUnrolling";
            if (mode.Equals(RecursionUnrollingModeType.NeverUnroll))
                return "NeverUnroll";
            throw new ArgumentException("Unknown unrolling mode");
        }

        public IdealValuesHandler(MethodInfo methodInfo, [CallerFilePath] string currentFilePath = "")
        {
            var currentFolder = Path.GetDirectoryName(currentFilePath);
            _idealValuePath = GetIdealValuePath(currentFolder, methodInfo);
            ExpectedValue = ReadIdealValue(_idealValuePath);
            _methodName = MethodInfoToString(methodInfo);
        }

        private static int MethodHash(MethodInfo methodInfo)
        {
            return methodInfo
                .GetParameters()
                .Concat(new []{methodInfo.ReturnParameter})
                .Select(p => p.ParameterType.ToString())
                .Aggregate(0, (current, t) =>
                    t.Aggregate(unchecked(current + t.Length), (h, c) => unchecked(h * 314159 + c)));
        }

        private static string GetIdealValuePath(string currentFolder, MethodInfo methodInfo)
        {
            var typeName = methodInfo?.DeclaringType?.FullName?.Split('.');
            if (typeName == null)
                return null;
            var methodName = $"{methodInfo.Name}.{MethodHash(methodInfo)}{IdealTestFileExtension}";
            var idealValuePath = Path.Combine(
                currentFolder,
                GoldsDirectoryName,
                _osGoldDirectoryName,
                IdealUnrollingOptionDirectoryName,
                Path.Combine(typeName),
                methodName);
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

    internal enum RecursionUnrollingMode
    {
        SmartUnrolling,
        NeverUnroll
    }

    internal class TestSvmAttribute : NUnitAttribute, IWrapTestMethod, ISimpleTestBuilder
    {
        private bool _ignoreTest;

        private static Dictionary<RecursionUnrollingModeType, RecursionUnrollingMode> _recursionUnrollingModes = new Dictionary<RecursionUnrollingModeType, RecursionUnrollingMode>
        {
            [RecursionUnrollingModeType.NeverUnroll] = RecursionUnrollingMode.NeverUnroll,
            [RecursionUnrollingModeType.SmartUnrolling] = RecursionUnrollingMode.SmartUnrolling
        };

        public TestSvmAttribute(params RecursionUnrollingMode[] options)
        {
            _ignoreTest = !options.Contains(_recursionUnrollingModes[Options.RecursionUnrollingMode()]);
        }

        public TestCommand Wrap(TestCommand command)
        {
            return new TestSvmCommand(command, _ignoreTest);
        }

        private class TestSvmCommand : DelegatingTestCommand
        {
            private bool _ignoreTest;

            public TestSvmCommand(TestCommand innerCommand, bool ignoreTest) : base(innerCommand)
            {
                _ignoreTest = ignoreTest;
            }

            public override TestResult Execute(TestExecutionContext context)
            {
                if (_ignoreTest)
                {
                    context.CurrentResult.SetResult(ResultState.Ignored, "Test doesn't match SVM options");
                    return context.CurrentResult;
                }

                var methodInfo = innerCommand.Test.Method.MethodInfo;
                var idealValue = new IdealValuesHandler(methodInfo);
                var gotValue = SVM.ExploreOne(methodInfo);
                var mode = $"// Explored in {IdealValuesHandler.IdealUnrollingOptionDirectoryName} mode\n";

                if (string.Equals(idealValue.ExpectedValue, gotValue))
                {
                    context.CurrentResult.SetResult(ResultState.Success, mode);
                }
                else
                {
                    idealValue.CreateTemporaryIdealFile(gotValue);
                    var diff = idealValue.DiffOfGotAndIdealValues(gotValue);
                    context.CurrentResult.SetResult(ResultState.Failure, mode + diff);
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
