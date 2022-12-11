import os
import re
from collections import defaultdict

EXAMPLES = {
    "part1": {
#         """\
# noop
# addx 3
# addx -5
# """: 0,
        """\
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
""": 13140,
    },
    "part2": {
        """\
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
""": None,
    },
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    return run_code(lines)

def part2(lines):
    run_code(lines, draw=True)

def run_code(lines, draw=False):
    x = 1
    cycle = 1
    pipeline = dict()
    signal_strength = 0
    crt = ""
    sprite = "".join("#" if abs(i-x) <= 1 else "." for i in range(40))
    #print(f"Sprite position: {sprite}")
    #print()
    while True:

        if len(pipeline) == 0:
            try:
                line = next(lines)
                if line == "":
                    continue
            except StopIteration:
                break
            # print(f"Start cycle {cycle:>3}: begin executing {line}")
            if line == "noop":
                pass
            if line.startswith("addx"):
                v = int(line.split(" ")[1])
                pipeline[cycle+1] = v

        # "during"
        pixel_pos = (cycle-1)%40
        # print(f"During cycle {cycle:>2}: CRT draws pixel in position {pixel_pos}")
        if cycle in [20,60,100,140,180,220]:
            signal_strength += cycle*x

        pixel = sprite[pixel_pos]
        crt += pixel
        crt_row = crt.split("\n")[-1]
        # print(f"Current CRT row: {crt_row}")
        if (cycle % 40) == 0:
            crt += "\n"

        # "after"
        if cycle in pipeline:
            x += pipeline[cycle]
            del pipeline[cycle]
            # print(f"End of cycle {cycle:>2}: finish executing {line} (Register X is now {x})")
            sprite = "".join("#" if abs(i-x) <= 1 else "." for i in range(40))
            # print(f"Sprite position: {sprite}")
        # print()
        cycle += 1
    if draw:
        print(crt)
    return signal_strength

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
