import os
import re
from collections import defaultdict
from time import sleep

EXAMPLES = {
    "part1": {
        """\
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
""": 24,
    }
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    cave, range_y, range_x = parse_input(lines)
    return simulate_sand(cave, range_y, range_x)

def part2(lines):
    pass

def parse_input(lines):
    cave = defaultdict(lambda: defaultdict(lambda: "."))
    min_y, max_y = 0, 0
    min_x, max_x = 500, 500
    cave[0][500] = "+"
    for line in lines:
        if line == "":
            continue
        points = [tuple(map(int, p.strip().split(","))) for p in line.split("->")]
        # draw lines connecting the points
        for i in range(1,len(points)):
            src_x, src_y = points[i-1][0], points[i-1][1]
            dst_x, dst_y = points[i][0], points[i][1]
            min_x = min(min_x, src_x, dst_x)
            max_x = max(max_x, src_x, dst_x)
            min_y = min(min_y, src_y, dst_y)
            max_y = max(max_y, src_y, dst_y)
            if src_x > dst_x or src_y > dst_y:
                # switch points
                src_x, src_y, dst_x, dst_y = dst_x, dst_y, src_x, src_y
            for y in range(src_y, dst_y+1):
                for x in range(src_x, dst_x+1):
                    cave[y][x] = "#"

    return cave, range(min_y, max_y+1), range(min_x, max_x+1)

def draw_cave(cave, range_y, range_x):
    s = ""
    for y in range_y:
        for x in range_x:
            #print(cave[y][x], end="")
            s += cave[y][x]
        s += "\n"
    print(s)

def simulate_sand(cave, range_y, range_x):
    resting_grains = 0
    falling_into_abyss = False
    while not falling_into_abyss:
        x,y = 500,0
        while y in range_y:
            y += 1
            if y not in range_y:
                falling_into_abyss = True
            if cave[y][x] == ".":
                continue
            if cave[y][x-1] == ".":
                x -= 1
                continue
            if cave[y][x+1] == ".":
                x += 1
                continue
            # no free position, so the grain is fixed, let the next one fall
            break
        if not falling_into_abyss:
            cave[y-1][x] = "o"
            resting_grains += 1
            # print()
            # draw_cave(cave, range(min(range_y)-1, max(range_y)+2), range(min(range_x)-1, max(range_x)+2))
            # print(resting_grains)
    return resting_grains


if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
