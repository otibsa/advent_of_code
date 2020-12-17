import os
import textwrap
import re

EXAMPLES = {
    "part1": {
        """\
        16
        10
        15
        5
        1
        11
        7
        19
        6
        12
        4
        """: 35,
        """\
        28
        33
        18
        42
        31
        14
        46
        20
        48
        47
        24
        23
        49
        45
        19
        38
        39
        11
        1
        32
        25
        35
        8
        17
        7
        9
        4
        2
        34
        10
        3
        """: 220
    },
    "part2": {
        """\
        16
        10
        15
        5
        1
        11
        7
        19
        6
        12
        4
        """: 8,
        """\
        28
        33
        18
        42
        31
        14
        46
        20
        48
        47
        24
        23
        49
        45
        19
        38
        39
        11
        1
        32
        25
        35
        8
        17
        7
        9
        4
        2
        34
        10
        3
        """:19208
    }
}

def get_lines(func):
    def wrapped(*args, **kwargs):
        return str(func([line for line in args[0].split("\n") if line != ""], *args[1:], **kwargs))
    return wrapped

@get_lines
def part1(lines):
    adapters = [int(i) for i in lines]
    target_joltage = max(adapters)+3
    diffs = [3]
    joltage = 0
    for a in sorted(adapters):
        diffs += [a-joltage]
        joltage = a
    return len([d for d in diffs if d == 1]) * len([d for d in diffs if d == 3])

@get_lines
def part2(lines):
    adapters = sorted(int(i) for i in lines)
    adapters = [0] + adapters + [adapters[-1]+3]
    diffs = [m-n for n,m in zip(adapters, adapters[1:])]
    count_ones = 0
    combinations = 1
    for d in diffs:
        if d == 1:
            count_ones += 1
        elif d == 3:
            combinations *= count_valid(count_ones)
            count_ones = 0
    return combinations

def count_valid(n):
    if n == 0:
        return 1
    if n == 1:
        return 1
    if n == 2:
        return 2
    if n == 3:
        return 4
    return 2*count_valid(n-1) - count_valid(n-4)

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
