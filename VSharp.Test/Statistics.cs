using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using CsvHelper;

namespace VSharp.Test
{
    class ExceptionInfo : IComparable<ExceptionInfo>
    {
        public string Type { get; }
        public string Message { get; }
        public string Location { get; }
        public int Count { get; }
        public string ExampleMethod { get; }
        public ExceptionInfo(string type, string message, string location, int count, string exampleMethod)
        {
            Type = type;
            Message = message;
            Location = location;
            Count = count;
            ExampleMethod = exampleMethod;
        }

        public int CompareTo(ExceptionInfo other)
        {
            if (ReferenceEquals(this, other)) return 0;
            if (ReferenceEquals(null, other)) return 1;
            var typeComparison = string.Compare(Type, other.Type, StringComparison.Ordinal);
            if (typeComparison != 0) return typeComparison;
            var countComparison = other.Count.CompareTo(Count);
            if (countComparison != 0) return countComparison;
            return string.Compare(Message, other.Message, StringComparison.Ordinal);
        }
    }

    public static class Statistics
    {
        private static int _methodsNumber;
        private static int _succeededMethodsNumber;
        private static Dictionary<string, Dictionary<string, Dictionary<string, List<string>>>> _allExceptions = new Dictionary<string, Dictionary<string, Dictionary<string, List<string>>>>();

        public static void SetupBeforeMethod(MethodBase m)
        {
            _methodsNumber++;
        }

        public static void AddSucceededMethod(MethodBase m)
        {
            Console.WriteLine("DONE: {0}", m);
            _succeededMethodsNumber += 1;
        }

        private static string MethodName(MethodBase m)
        {
            return $"{m.DeclaringType.FullName}.{m.Name}";
        }

        public static void AddException(Exception e, MethodBase m)
        {
            var type = e.GetType().Name;
            var eMessage = e.Message;
            var frame = new StackTrace(e, true).GetFrame(0);
            var line = frame.GetFileLineNumber();
            var eLocation = $"{MethodName(frame.GetMethod())} at line {line}";

            if (!_allExceptions.ContainsKey(type))
                _allExceptions[type] = new Dictionary<string,Dictionary<string, List<string>>>();

            var typedExceptions = _allExceptions[type];
            if (!typedExceptions.ContainsKey(eMessage))
                typedExceptions[eMessage] = new Dictionary<string, List<string>>();

            var typedExceptionsWithMessage = typedExceptions[eMessage];
            if (!typedExceptionsWithMessage.ContainsKey(eLocation))
                typedExceptionsWithMessage[eLocation] = new List<string>();

            typedExceptionsWithMessage[eLocation].Add(Reflection.GetFullMethodName(m));
        }

        private static List<ExceptionInfo> AllExceptionsDictionaryToExceptionInfos()
        {
            var res = new List<ExceptionInfo>();
            foreach (var (type, typedExceptions) in _allExceptions)
                foreach (var (message, typedExceptionsWithMessage) in typedExceptions)
                    foreach (var (location, methods) in typedExceptionsWithMessage)
                        res.Add(new ExceptionInfo(type, message, location, methods.Count, methods.First()));
            res.Sort();
            return res;
        }

        private static string SaveExceptionsShortStats(List<ExceptionInfo> exceptionInfos)
        {
            var filename = Path.ChangeExtension(Path.GetTempFileName(), "csv");
            using var writer = new StreamWriter(filename);
            using var csv = new CsvWriter(writer, CultureInfo.InvariantCulture);
            csv.WriteHeader<ExceptionInfo>();
            csv.Flush();
            writer.WriteLine();
            csv.WriteRecords(exceptionInfos);
            return filename;
        }

        private static void PrintLine()
        {
            Console.WriteLine("<--------------------------------------------------------------------------------------------------->");
        }

        public static void PrintExceptionsStats()
        {
            var exceptionInfos = AllExceptionsDictionaryToExceptionInfos();
            Console.WriteLine($"STATISTICS: Total methods number: {_methodsNumber}");
            Console.WriteLine($"STATISTICS: Succeeded methods number: {_succeededMethodsNumber}");
            Console.WriteLine($"Exploration statistics has been saved in {SaveExceptionsShortStats(exceptionInfos)}");
            PrintLine();

            foreach (var exceptionInfo in exceptionInfos)
            {
                Console.WriteLine($"STATISTICS: {exceptionInfo.Message}");
                Console.WriteLine($"STATISTICS: Occured in {exceptionInfo.Location}");
                Console.WriteLine($"STATISTICS: Number of occurrences: {exceptionInfo.Count}");
                Console.WriteLine($"STATISTICS: Method for debugging: {exceptionInfo.ExampleMethod}");
                PrintLine();
            }
        }
    }
}
