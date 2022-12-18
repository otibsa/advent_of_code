import os
import re
from collections import defaultdict
from math import inf

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
    "part2": {
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
""": 58,
    },
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    _, area, _, _, _ = parse_input(lines)
    return area

def part2(lines):
    # run BFS starting outside a cube and mark all reached Air cubes with "+"
    # for steam. This should leave air pockets untouched, then we can change all
    # remaining air cubes to lava cubes, add them to the input list and run the
    # area calculation again
    lines = list(lines)
    cubes, _, range_x, range_y, range_z = parse_input(lines)
    cubes = flood(cubes, range_x, range_y, range_z)
    lines = [",".join(map(str, c)) for c in cubes.keys() if cubes[c] in ".#"]
    _, area, _, _, _ = parse_input(lines)
    return area

def flood(cubes, range_x, range_y, range_z):
    deltas = [(-1,0,0), (1,0,0), (0,-1,0), (0,1,0), (0,0,-1), (0,0,1)]
    min_x, max_x = min(range_x)-1, max(range_x)+1
    min_y, max_y = min(range_y)-1, max(range_y)+1
    min_z, max_z = min(range_z)-1, max(range_z)+1
    queue = [(min_x, min_y, min_z)]
    while queue:
        v = queue[0]
        queue = queue[1:]
        for n in [vector_add(v, d) for d in deltas]:
            nx,ny,nz = n
            if nx<min_x or nx>max_x or ny<min_y or ny>max_y or nz<min_z or nz>max_z:
                continue
            if cubes[n] == ".":
                cubes[n] = "+"
                queue += [n]
    return cubes

def draw_cubes(cubes, range_x, range_y, range_z):
    for z in range_z:
        print(f"{z=}")
        for y in range_y:
            for x in range_x:
                c = cubes[(x,y,z)]
                #c = {"A": ".", "L": "#", "S": "+"}[c]
                print(c, end="")
            print()
        print()

def parse_input(lines):
    cubes = defaultdict(lambda: ".")
    area = 0
    deltas = [(-1,0,0), (1,0,0), (0,-1,0), (0,1,0), (0,0,-1), (0,0,1)]
    min_x, max_x, min_y, max_y, min_z, max_z = inf, -inf, inf, -inf, inf, -inf
    for line in lines:
        if line == "":
            continue
        cube = tuple(map(int, line.split(",")))
        x,y,z = cube
        min_x, min_y, min_z = min(min_x, x), min(min_y, y), min(min_z, z)
        max_x, max_y, max_z = max(max_x, x), max(max_y, y), max(max_z, z)
        touching = sum(cubes[n]!="." for n in [vector_add(cube, d) for d in deltas])
        area += 6-2*touching
        cubes[cube] = "#"
    range_x = range(min_x, max_x+1)
    range_y = range(min_y, max_y+1)
    range_z = range(min_z, max_z+1)

    # ? Apparently the cube coordinates want to be initialized for the flooding
    # to work
    _ = [cubes[(x,y,z)] for z in range_z for y in range_y for x in range_x]
    return cubes, area, range_x, range_y, range_z

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
