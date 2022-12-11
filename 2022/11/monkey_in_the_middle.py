import os
import re
from collections import defaultdict
from copy import deepcopy

EXAMPLES = {
    "part1": {
        """\
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1""": 10605
    },
}

class Monkey:
    def __init__(self, starting_items, operation_tuple, divisible_by, throw_true, throw_false):
        self.items = starting_items
        self.operation_tuple = operation_tuple
        self.divisible_by = divisible_by
        self.throw_true = throw_true
        self.throw_false = throw_false
        self.inspections = 0

    def __repr__(self):
        return ", ".join(str(i) for i in self.items)

    def do_operation(self, old):
        op, value = self.operation_tuple
        if value == "old":
            value = old
        else:
            value = int(value)
        if op == "+":
            return old + value
        if op == "*":
            return old * value

    def do_turn(self, monkeys, debug=True):
        next_monkey = None
        for worry_level in self.items.copy():
            if debug:
                print(f"  Monkey inspects an item with a worry level of {worry_level}.")
            self.inspections += 1
            worry_level = self.do_operation(worry_level)
            if debug:
                print(f"    Worry level is {self.operation_tuple[0]} by {self.operation_tuple[1]} to {worry_level}.")
            worry_level = worry_level // 3
            if debug:
                print(f"    Monkey gets bored with item. Worry level is divided by 3 to {worry_level}.")
            is_div = worry_level % self.divisible_by == 0
            if debug:
                print(f"    Current worry level is{' not' if is_div else ''} divisible by {self.divisible_by}.")
            if is_div:
                next_monkey = self.throw_true
            else:
                next_monkey = self.throw_false
            if debug:
                print(f"    Item with worry level of {worry_level} is thrown to monkey {next_monkey}")
            monkeys[next_monkey].items += [worry_level]
            self.items = self.items[1:]

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    monkeys = parse_input(lines)
    return run_game(monkeys, debug=False)

def part2(lines):
	pass

def run_game(monkeys, debug=False):
    for r in range(20):
        for i, m in monkeys.items():
            if debug:
                print(f"Monkey {i}:")
            m.do_turn(monkeys, debug=False)
        if debug:
            print()
            print(f"After round {r+1}:")
            for i, m in monkeys.items():
                print(f"Monkey {i}: {m}")
            print()
    if debug:
        for i, m in monkeys.items():
            print(f"Monkey {i} inspected items {m.inspections} times")
    inspections = sorted(m.inspections for m in monkeys.values())
    monkey_business = inspections[-2]*inspections[-1]
    return monkey_business

def parse_input(lines):
    monkeys = {}
    for line in list(lines)+[""]:
        # print(line)
        if m := re.match(r"^Monkey ([0-9]+)", line):
            i = int(m.group(1))
        elif m := re.match(r".*Starting items: (.*)", line):
            starting_items = [int(s.strip()) for s in m.group(1).split(",")]
        elif m := re.match(r".*Operation: new = old ([+*]) (.*)", line):
            operation_tuple = (m.group(1), m.group(2))
        elif m := re.match(r".*Test: divisible by ([0-9]+)", line):
            divisible_by = int(m.group(1))
        elif m := re.match(r".*If true: throw to monkey ([0-9]+)", line):
            throw_true = int(m.group(1))
        elif m := re.match(r".*If false: throw to monkey ([0-9]+)", line):
            throw_false = int(m.group(1))
        elif line == "":
            monkeys[i] = Monkey(starting_items, operation_tuple, divisible_by, throw_true, throw_false)
        else:
            print("unknown line", line)
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
