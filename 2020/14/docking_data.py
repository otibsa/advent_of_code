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
    "part2": {
        """\
        mask = 000000000000000000000000000000X1001X
        mem[42] = 100
        mask = 00000000000000000000000000000000X0XX
        mem[26] = 1
        """: 208
    }
}

def get_lines(func):
    def wrapped(*args, **kwargs):
        return str(func([line for line in args[0].split("\n") if line != ""], *args[1:], **kwargs))
    return wrapped

@get_lines
def part1(lines):
    state = {
        "mask": "X"*36,
        "mem": {}
    }
    run(state, lines)
    return sum(state["mem"].values())

@get_lines
def part2(lines):
    state = {
        "mask": "X"*36,
        "mem": {}
    }
    run(state, lines, version=2)
    return sum(state["mem"].values())

def run(state, program, version=1):
    for instruction in program:
        if m := re.match(r"^mask = (.*)$", instruction):
            state["mask"] = m.group(1)
        elif m:= re.match(r"mem\[([0-9]+)\] = ([0-9]+)", instruction):
            addr = int(m.group(1))
            value = int(m.group(2))
            set_mask = int(state["mask"].replace("X", "0"), 2)
            clear_mask = int(state["mask"].replace("X", "1"), 2)
            if version == 1:
                value |= set_mask
                value &= clear_mask
                state["mem"][addr] = value
            elif version == 2:
                addr |= set_mask
                addresses = [addr]
                for i, b in enumerate(state['mask'][::-1]):
                    if b == "X":
                        # for each address already in the list, flip the bit,
                        # and add the new address to the list
                        addresses += [a^(1<<i) for a in addresses]
                for a in addresses:
                    state["mem"][a] = value
        else:
            print(f"Unknown instruction: {instruction}")

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
