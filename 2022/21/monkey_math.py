import os
import re
from collections import defaultdict
from math import inf
from pprint import pprint

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
    pass

def dfs(monkeys, m):
    if type(monkeys[m]) == int:
        return monkeys[m]
    left = monkeys[m][0]
    right = monkeys[m][2]
    left = dfs(monkeys, left)
    right = dfs(monkeys, right)
    op = monkeys[m][1]
    if op == "+":
        return left + right
    elif op == "-":
        return left - right
    elif op == "*":
        return left * right
    elif op == "/":
        return left / right

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
