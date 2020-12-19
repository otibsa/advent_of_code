import sys
import os
import textwrap
import re
import math

EXAMPLES = {
    "part1": {
        """\
        939
        7,13,x,x,59,x,31,19
        """: 295
    },
}

def get_lines(func):
    def wrapped(*args, **kwargs):
        return str(func([line for line in args[0].split("\n") if line != ""], *args[1:], **kwargs))
    return wrapped

@get_lines
def part1(lines):
    timestamp = int(lines[0])
    bus_ids = lines[1].split(",")
    bus_ids = [int(i) for i in bus_ids if i != "x"]
    i, delay = find_bus(timestamp, bus_ids)
    return i*delay

@get_lines
def part2(lines):
    pass

def find_bus(timestamp, bus_ids):
    next_arrivals = {i: i*math.ceil(timestamp/i) for i in bus_ids}
    next_bus = min(next_arrivals, key=next_arrivals.get)
    return next_bus, next_arrivals[next_bus]-timestamp

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
