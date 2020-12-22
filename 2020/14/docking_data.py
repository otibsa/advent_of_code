import sys
import os
import textwrap
import re
import math

EXAMPLES = {
    "part1": {
        """\
        mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
        mem[8] = 11
        mem[7] = 101
        mem[8] = 0
        """: 165
    },
}

def get_lines(func):
    def wrapped(*args, **kwargs):
        return str(func([line for line in args[0].split("\n") if line != ""], *args[1:], **kwargs))
    return wrapped

@get_lines
def part1(lines):
    state = {
        "clear_mask": 2*36-1,
        "set_mask": 0,
        "mem": {}
    }
    run(state, lines)
    return sum(state["mem"].values())

def run(state, program):
    for instruction in program:
        if m := re.match(r"^mask = (.*)$", instruction):
            state["set_mask"] = int(m.group(1).replace("X", "0"), 2)
            state["clear_mask"] = int(m.group(1).replace("X", "1"), 2)
        elif m:= re.match(r"mem\[([0-9]+)\] = ([0-9]+)", instruction):
            addr = int(m.group(1))
            value = int(m.group(2))
            tmp = value
            value |= state["set_mask"]
            value &= state["clear_mask"]
            state["mem"][addr] = value
        else:
            print(f"Unknown instruction: {instruction}")

@get_lines
def part2(lines):
    pass

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
