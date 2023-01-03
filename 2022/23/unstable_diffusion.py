import os
import re
from collections import defaultdict
from time import sleep, time
from pprint import pprint

EXAMPLES = {
    "part1": {
        """\
.....
..##.
..#..
.....
..##.
.....
""": 25,
        """\
....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
""": 110
    },
}

directions = ">v<^"

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    grid, elves, max_y, max_x = parse_input(lines)
    #draw_grid(grid, range(max_y+1), range(max_x+1))
    grid, elves = move_elves(grid, elves, 10)
    min_y, min_x = list(elves.keys())[0]
    max_y, max_x = list(elves.keys())[0]
    for y,x in elves:
        max_y = max(max_y, y)
        max_x = max(max_x, x)
        min_y = min(min_y, y)
        min_x = min(min_x, x)
    #print(f"{max_y+1-min_y} x {max_x+1-min_x}")
    return (max_y+1-min_y)*(max_x+1-min_x)-len(elves)

def part2(lines):
    pass

def move_elves(grid, elves, rounds):
    elves = {e: None for e in elves}
    first_direction = 0
    while rounds > 0:
        proposed = defaultdict(int)
        deltas = [
            [(-1,-1), (-1, 0), (-1, 1)],  # NW, N, NE
            [( 1,-1), ( 1, 0), ( 1, 1)],  # SW, S, SE
            [(-1,-1), ( 0,-1), ( 1,-1)],  # NW, W, SW
            [(-1, 1), ( 0, 1), ( 1, 1)],  # NE, E, SE
        ]
        # propose moves
        for (y,x), _ in elves.items():
            # count neighbors
            n = sum((y+dy, x+dx) in elves for d in deltas for dy,dx in d)
            if n == 0:
                continue
            order = list(range(first_direction, 4))+list(range(first_direction))
            checks = [deltas[i] for i in order]
            for c in checks:
                if all((y+dy, x+dx) not in elves for (dy,dx) in c):
                    # no elf in any of the three locations, so we propose to
                    # move there.
                    p = (y+c[1][0], x+c[1][1])
                    # count how many elves propose this location
                    proposed[p] += 1
                    # for each elf keep track of their proposed move
                    elves[y,x] = p
                    # don't propose any other moves
                    break
        elves_moved = 0
        # move
        for (y,x), p in list(elves.items()):
            if proposed[p] == 1:
                elves_moved += 1
                elves[p] = None
                grid[p[0]][p[1]] = grid[y][x]
                grid[y][x] = "."
                del elves[(y,x)]
            else:
                # forget about the proposed move
                elves[(y,x)] = None

        first_direction = (first_direction+1)%4
        if elves_moved == 0:
            break
        rounds -= 1
        #print(f"T-{rounds}: ")
        #draw_grid(grid, range(min_y, max_y+1), range(min_x, max_x+1))
    return grid, elves

def draw_grid(grid, range_y, range_x):
    s = "    "
    for x in range_x:
        if x%10 == 0:
            s += f"{x//10:2}"
        elif x%10 == 9:
            pass
        else:
            s += " "
    s += "\n    "
    for x in range_x:
        s += str(x%10)
    s += "\n"
    for y in range_y:
        s += f"{y:3} "
        #min_x = min(x for x in grid[y].keys() if grid[y][x] == "#")
        #s += "."*min_x
        for x in range_x:
            #print(cave[y][x], end="")
            #if grid[y][x] != " ":
            s += grid[y][x]
        s += "\n"
    print(s)

def parse_input(lines):
    grid = defaultdict(lambda: defaultdict(lambda: "."))
    elves = set()
    max_x, max_y = 0, 0
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            if c == "#":
                max_x = max(max_x, x)
                max_y = max(max_y, y)
                grid[y][x] = "#"
                elves |= {(y,x)}

    return grid, elves, max_y, max_x

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))

