import sys
import os
import textwrap
import re
import math
from collections import defaultdict
from copy import deepcopy

EXAMPLES = {
    "part1": {
        """\
        .#.
        ..#
        ###
        """: 112
    },
}

def get_lines(func):
    def wrapped(*args, **kwargs):
        return str(func([line for line in args[0].split("\n") if line != ""], *args[1:], **kwargs))
    return wrapped

def get_size(cube):
    min_z,max_z = min(cube.keys()), max(cube.keys())
    min_y,max_y = 0,0
    min_x,max_x = 0,0
    for z in range(min_z, max_z+1):
        min_y = min(min_y, *cube[z].keys())
        max_y = max(max_y, *cube[z].keys())
        for y in range(min_y, max_y+1):
            min_x = min(min_x, min_x, *cube[z][y].keys())
            max_x = max(max_x, max_x, *cube[z][y].keys())
    return range(min_z,max_z+1),range(min_y,max_y+1),range(min_x,max_x+1)

def print_cube(cube):
    range_z, range_y, range_x = get_size(cube)
    for z in range_z:
        print(f"{z=}")
        for y in range_y:
            for x in range_x:
                if cube[z][y][x]:
                    print("#", end="")
                else:
                    print(".", end="")
            print()
        print()

def parse_input(lines):
    cube = defaultdict(lambda: defaultdict(lambda: defaultdict(bool)))
    z=0
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            if c=="#":
                cube[z][y][x] = True
    return cube

def count_neighbors(cube, x,y,z):
    neighbors = 0
    for nz in [z-1, z, z+1]:
        for ny in [y-1, y, y+1]:
            for nx in [x-1, x, x+1]:
                if nz==z and ny==y and nx==x:
                    continue
                if cube[nz][ny][nx]:
                    neighbors += 1
    return neighbors

def conway_step(cube):
    tmp_cube = deepcopy(cube)
    range_z, range_y, range_x = get_size(cube)
    for z in range(range_z.start-1, range_z.stop+1):
        for y in range(range_y.start-1, range_y.stop+1):
            for x in range(range_x.start-1, range_x.stop+1):
                n = count_neighbors(cube, x,y,z)
                if cube[z][y][x]:
                    if n not in [2,3]:
                        tmp_cube[z][y][x] = False
                if not cube[z][y][x] and n == 3:
                    tmp_cube[z][y][x] = True
    return tmp_cube

@get_lines
def part1(lines):
    cube = parse_input(lines)
    # print_cube(cube)
    for i in range(6):
        cube = conway_step(cube)
        # print(f"After {i+1} cycle(s):")
        # print_cube(cube)
        # print()

    nr_active = 0
    range_z, range_y, range_x = get_size(cube)
    for z in range_z:
        for y in range_y:
            nr_active += sum(cube[z][y].values())
    return nr_active

@get_lines
def part2(lines):
    pass

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            output = globals()[func_name](textwrap.dedent(input))
            assert output == str(expected_output), f"got {output}, expected {expected_output}"

if __name__ == "__main__":
    test_examples()
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
