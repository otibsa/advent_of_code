import os
import re
from collections import defaultdict

EXAMPLES = {
    "part1": {
        """\
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
""": "CMZ"
    },
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    stacks = parse_stacks(lines)
    return "".join(stacks[i][0] for i in range(len(stacks.keys())))

def part2(lines):
    pass

def parse_stacks(lines):
    stacks = defaultdict(list)
    order_func = list if crane_model == "9001" else reversed
    for line in lines:
        for i, m in enumerate(re.finditer(r"\[(.)", line)):
            stacks[m.start()//4] += [m.group(1)]
        if m := re.match(r"move (.+) from (.+) to (.+)", line):
            n, source, dest = map(int, m.groups())
            stacks[dest-1] = list(reversed(stacks[source-1][:n]))+stacks[dest-1]
            stacks[source-1] = stacks[source-1][n:]
    return stacks

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
