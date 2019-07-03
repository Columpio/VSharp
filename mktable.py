#!/usr/bin/env python3
import os
import re
import sys
from typing import Dict, List, DefaultDict, Tuple
from itertools import chain
from collections import defaultdict

class bcolors:
	PINK = '\033[95m'
	BLUE = '\033[94m'
	GREEN = '\033[92m'
	YELLOW = '\033[93m'
	RED = '\033[91m'
	ENDC = '\033[0m'

class ColoredText:
	def __init__(self, s, color, time=None):
		self.text = s
		self.color = color
		self.colorPattern = "{text}" if color is None else "{color}{{text}}{end}".format(color=color, end=bcolors.ENDC)
		self.puretime = time
		self.time = "" if time is None else "({time})".format(time=time)
	
	def __str__(self):
		return self.text + self.time

	def __len__(self):
		return len(str(self))
	
	def colored(self):
		return self.colorPattern.format(text=str(self))
	
	def __repr__(self):
		return str(self)
	
	def __eq__(self, other):
		return self.text == other.text


def rightRes(s):
	return ColoredText(s.text, bcolors.GREEN, s.puretime)

def no_result():
	return ColoredText("-", None)

def strToRes(s):
	n = None
	m = re.match(r"^\(([\w\s]+)\, (\d+)\)$", s)
	if m:
		s, n = m.groups()[0], m.groups()[1]
	if "Sat" in s:
		return ColoredText("sat", None, n)
	elif "Unsat" in s:
		return ColoredText("unsat", None, n)
	elif "Time limit" in s:
		return ColoredText("TL", bcolors.BLUE)
	elif "z3: " in s:
		return ColoredText("z3 :(", bcolors.BLUE)
	return no_result()

def readResultFile(filename : str) -> DefaultDict[str, ColoredText]:
	results = {}
	with open(filename) as file:
		for line in file:
			solver, _, res = line.partition('\t')
			results[solver] = strToRes(res)
	new_results = defaultdict(no_result)
	if "Human" in results:
		goldResult = results["Human"]
		for k, v in results.items():
			new_results[k] = rightRes(v) if v == goldResult else v
	else:
		new_results.update(results)
	return new_results

def testResults(testFolder : str) -> List[DefaultDict[str, ColoredText]]:
	queries = {}
	for queryFile in os.scandir(testFolder):
		if queryFile.name.endswith(".results"):
			n = int(queryFile.name.partition('.')[0])
			queries[n] = queryFile.path

	queries = list(queries.items())
	queries.sort(key=lambda t: t[0])
	queries = [t[1] for t in queries]
	results = list(map(readResultFile, queries))
	return results

def generateHeader(tests : Dict[str, List[DefaultDict[str, ColoredText]]]) -> List[str]:
	tests = [[set(x.keys()) for x in t] for t in tests.values()]
	solvers = list(set().union(*chain.from_iterable(tests)))
	solvers.remove("Human")
	solvers.sort()
	solvers.append("Human")
	return solvers

def shapeHeader(header : List[str], tests : Dict[str, List[DefaultDict[str, ColoredText]]]) -> Dict[str, List[List[ColoredText]]]:
	def shapeTest(test : DefaultDict[str, str]):
		return [test[solver] for solver in header]
	new_tests = {}
	for k, v in tests.items():
		new_tests[k] = list(map(shapeTest, v))
	return new_tests

def propagateTestName(tests : Dict[str, List[List[ColoredText]]]) -> List[List[ColoredText]]:
	new_tests = []
	for name, test in tests.items():
		for i, t in enumerate(test):
			new_tests.append([ColoredText(name, None), ColoredText(str(i), None)] + t)
	return new_tests

def fillColumnsWithSpaces(header : List[str], tests : List[List[ColoredText]]) -> List[str]:
	minWidths = list(map(len, header))
	for line in tests:
		for i, word in enumerate(line):
			minWidths[i] = max(minWidths[i], len(word))
	
	for line in tests:
		for i, word in enumerate(line):
			word.text = word.text.rjust(minWidths[i] - len(word.time))
	header = [h.rjust(minWidths[i]) for i, h in enumerate(header)]
	return header

def collectStatistics(solvers : List[str], tests : List[List[ColoredText]]):
	matched_results = {solver: 0 for solver in solvers}
	coverage = 0
	for row in tests:
		coverage += row.count(row[-1]) > 1
		for i, solver in enumerate(solvers, 2):
			if row[i] == row[-1]:
				matched_results[solver] += 1
	matched_results.pop(solvers[-1])  # remove Human solver
	best_solver, best_solver_score = max(matched_results.items(), key=lambda t: t[1])
	total_queries = len(tests)
	return coverage, best_solver, best_solver_score, total_queries

def makeTable(prefix: str, tests : Dict[str, List[DefaultDict[str, ColoredText]]]):
	solvers = generateHeader(tests)
	tests = shapeHeader(solvers, tests)
	tests = propagateTestName(tests)

	coverage, best_solver, best_solver_score, total_queries = collectStatistics(solvers, tests)

	header = ["Test name", "query"] + solvers
	header = fillColumnsWithSpaces(header, tests)

	header = ' '.join(header)
	line = '-' * len(header)
	tests.sort(key=lambda r: r[0].text.strip())
	table = '\n'.join(' '.join(res.colored() for res in row) for row in tests)
	
	n = len(header) - len(prefix)
	d, m = divmod(n, 2)
	l, r = d + m, d
	print('=' * l, prefix, '=' * r, sep='')

	print(header)
	print(line)
	print(table)
	print(line)
	print("Total queries:\t\t", total_queries, sep='')
	print("Solver coverage:\t", coverage, sep='')
	print("Best %s with score:\t%s" % (best_solver, best_solver_score))
	
def readTestFolderResults(folder: str) -> Dict[str, List[DefaultDict[str, ColoredText]]]:
	tests = {}
	for filename in os.scandir(folder):
		if filename.is_dir():
			testName = filename.name.rsplit('.', 2)[0]
			tests[testName] = testResults(filename.path)
	return tests

def printTestFolderResults(folder: str):
	tests = readTestFolderResults(folder)
	makeTable(folder, tests)
	print()

def printTeXTable(folder: str):
	def resultToRussian(result: str) -> str:
		if result == "sat":
			return "Небезопасно"
		elif result == "unsat":
			return "Безопасно"
		raise NotImplementedError

	def texrow(test_name: str, query_number: int, solver_result: str, gold_result: str, time: str):
		query_number = str(query_number)
		prefix = r"\rowcolor{red} " if solver_result != gold_result else ""
		solver_result = resultToRussian(solver_result)
		gold_result = resultToRussian(gold_result)
		row = [test_name, query_number, solver_result, gold_result, time]
		return prefix + " & ".join(row) + r" \\"
	
	tests = readTestFolderResults(folder)
	postfix = "Unsafe" if "unsafe" in folder.lower() else "Safe" if "safe" in folder.lower() else ""
	for test_name, queries in tests.items():
		test = [texrow(test_name+postfix, i, q["r_type"].text, q["Human"].text, q["r_type"].puretime) for i, q in enumerate(queries, start=1)]
		for q in test:
			print(q)


if len(sys.argv) > 1:
	folders = sys.argv[1:]
else:
	folders = ["ListWorking"]

for folder in folders:
	folder = os.path.join("VSharp.Test", "Golds", "VSharp", "Test", "Tests", folder)
	# printTeXTable(folder)
	printTestFolderResults(folder)
