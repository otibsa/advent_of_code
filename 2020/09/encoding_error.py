import os
import textwrap
import re

EXAMPLES = {
    "part1": {
        """\
        35
        20
        15
        25
        47
        40
        62
        55
        65
        95
        102
        117
        150
        182
        127
        219
        299
        277
        309
        576
        """: 127
    }
}

def get_lines(func):
    def wrapped(*args, **kwargs):
        return str(func([line for line in args[0].split("\n") if line != ""], *args[1:], **kwargs))
    return wrapped

@get_lines
def part1(lines, preamble_length=25):
    return find_error([], [int(i) for i in lines], preamble_length)

def find_error(preamble, xs, preamble_length):
    diff = preamble_length - len(preamble)
    if diff > 0:
        preamble += xs[:diff]
        xs = xs[diff:]
    x = xs[0]
    sums = []
    for i, n in enumerate(preamble):
        for m in preamble[i:]:
            sums += [n+m]
    if x in sums:
        return find_error(preamble[1:]+[x], xs[1:], preamble_length)
    return x

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input), preamble_length=5) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
