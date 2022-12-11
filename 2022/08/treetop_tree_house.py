import os
import re
from collections import defaultdict
from pprint import pprint

EXAMPLES = {
    "part1": {
        """\
30373
25512
65332
33549
35390
""": 21
    },
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    tree_map = parse_input(lines)
    visible_trees = find_visible(tree_map)
    return len(visible_trees)

def part2(lines):
    pass

def find_visible(tree_map):
    # assume square map
    n = len(tree_map)
    visible = dict()
    for x in range(n):
        # [from north, from south]
        for direction_range in [range(n), range(n-1,-1,-1)]:
            ceiling = -1
            for y in direction_range:
                h = tree_map[y][x]
                if h > ceiling:
                    if (x,y,h) not in visible:
                        visible[(x,y,h)] = 0
                    visible[(x,y,h)] += 1  # visible from one more direction
                    ceiling = h

    for y in range(n):
        # from west, from east
        for direction_range in [range(n), range(n-1,-1,-1)]:
            ceiling = -1
            for x in direction_range:
                h = tree_map[y][x]
                if h > ceiling:
                    if (x,y,h) not in visible:
                        visible[(x,y,h)] = 0
                    visible[(x,y,h)] += 1  # visible from one more direction
                    ceiling = h

    return visible

def parse_input(lines):
    tree_map = []
    for line in lines:
        if line == "":
            continue
        row = [int(c) for c in line]
        tree_map += [row]
    return tree_map

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
