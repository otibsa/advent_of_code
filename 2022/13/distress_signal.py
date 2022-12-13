import os
import re
from collections import defaultdict
import json

EXAMPLES = {
    "part1": {
        """\
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
""": 13,
        """\
[[4, 4], 4, 4, 4]
[[4, 4], 4, 4]
""": 0  # p1,p2 are in the right order (as in the example input), then p2,p1 must be in the wrong order
    }
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def packet_pair_gen(line_gen):
    for line in line_gen:
        if line == "":
            continue
        packet1 = json.loads(line)
        packet2 = json.loads(next(line_gen))
        yield (packet1, packet2)

def part1(lines):
    return sum(i+1 for i, (p1,p2) in enumerate(packet_pair_gen(lines)) if cmp_packets(p1,p2)<0)

def part2(lines):
    pass

def sign(x):
    return 0 if x == 0 else x//abs(x)

def cmp_packets(left, right, debug=False, indent=""):
    # cmp returns -1 if left < right, 0 if left == right, 1 if left > right
    # -1: right order
    # 1: not right order
    # 0: continue checking
    if type(left) == type(right) == int:
        return sign(left-right)
    elif type(left) == type(right) == list:
        for i in range(len(left)):
            if i >= len(right):
                # right ran out
                return 1
            c = cmp_packets(left[i], right[i], indent=indent+"  ")
            if c == 0:
                continue
            # got a result
            return c
        if len(left) == len(right):
            # no decision, and same length: "next part of the input"
            return 0
        # left ran out
        return -1
    if type(left) == int:
        return cmp_packets([left], right, indent=indent+"  ")
    if type(right) == int:
        return cmp_packets(left, [right], indent=indent+"  ")


if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
