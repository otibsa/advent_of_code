import os
import re
from collections import defaultdict

EXAMPLES = {
    "part1": {
        """\
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
""": 64,
    },
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    return calc_surface_area(lines)

def part2(lines):
    pass

def calc_surface_area(lines):
    cubes = defaultdict(bool)
    area = 0
    for line in lines:
        if line == "":
            continue
        cube = tuple(map(int, line.split(",")))
        deltas = [(-1,0,0), (1,0,0), (0,-1,0), (0,1,0), (0,0,-1), (0,0,1)]
        touching = sum(cubes[n] for n in [vector_add(cube, d) for d in deltas])
        area += 6-2*touching
        cubes[cube] = True
    return area

def vector_add(v1, v2):
    return tuple(map(sum, zip(v1,v2)))

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
