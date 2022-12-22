import os
import re
from collections import defaultdict
from math import inf
from pprint import pprint
from time import sleep

EXAMPLES = {
    "part1": {
        """\
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
""": 152,
    },
    "part2": {
        """\
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
""": 301
    }
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    monkeys = parse_input(lines)
    root_number = dfs(monkeys, "root")
    return int(root_number)

def part2(lines):
    monkeys = parse_input(lines)
    left_m, _, right_m = monkeys["root"]
    monkeys["humn"] = "x"
    left = dfs(monkeys, left_m)
    right = dfs(monkeys, right_m)
    if type(left) in [int, float]:
        to_solve, solution = right, left
    else:
        to_solve, solution = left, right
    s = solve(to_solve, solution)
    return s

def solve(to_solve, solution):
    if to_solve == "x":
        return solution
    left, op, right = to_solve
    if type(right) == int:
        if op == "+":
            return solve(left, solution - right)
        elif op == "-":
            return solve(left, solution + right)
        elif op == "*":
            return solve(left, solution // right)
        elif op == "/":
            return solve(left, solution * right)
    elif type(left) == int:
        #   20 / (X - 2) == 2
        if op == "+":
            return solve(right, solution - left)
        elif op == "-":
            return solve(right, left - solution)
        elif op == "*":
            return solve(right, solution // left)
        elif op == "/":
            return solve(right, left // solution)

def dfs(monkeys, m):
    if type(monkeys[m]) in [int,float] or monkeys[m] == "x":
        return monkeys[m]
    left, op, right = monkeys[m]
    left = dfs(monkeys, left)
    right = dfs(monkeys, right)
    if type(left) in [int,float] and type(right) in [int,float]:
        if op == "+":
            return left + right
        elif op == "-":
            return left - right
        elif op == "*":
            return left * right
        elif op == "/":
            return left // right
    return (left, op, right)

def parse_input(lines):
    monkeys = {}
    for line in lines:
        if m := re.match(r"(.*): (.*) ([-+*/]) (.*)", line):
            name, left, op, right = m.groups()
            monkeys[name] = (left, op, right)
        elif m := re.match(f"(.*): ([0-9]+)", line):
            name, number = m.groups()
            monkeys[name] = int(number)
    return monkeys

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
