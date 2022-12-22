import os
import re
from collections import defaultdict
from time import sleep, time

EXAMPLES = {
    "part1": {
        """\
        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.

10R5L5R10L4R5L5
""": 6032,
        """\
        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
""": 6032
    },
}

#directions = ["east", "south", "west", "north"]
directions = ">v<^"

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    grid, path, max_y, max_x = parse_input(lines)
    # draw_grid(grid, max_y, max_x)
    y = 0
    x = min(x for x in grid[y].keys() if grid[y][x] == ".")
    facing = 0
    y, x, facing = walk(grid, path, y, x, facing, max_y, max_x)
    # print(f"Finish walking at {y=} {x=} {facing=}")
    return 1000*(y+1) + 4*(x+1) + facing

def part2(lines):
    pass

def walk(grid, path, y, x, facing, max_y, max_x):
    grid[y][x] = directions[facing]
    #print(f"Start walking at {y=} {x=} facing {directions[facing]}")
    for step in path:
        if type(step) == str:
            facing += 1 if step == "R" else -1
            facing = facing % 4
            #print(f"Turning {step}, now facing {directions[facing]}")
            grid[y][x] = directions[facing]
            continue
        deltas = [(0,1), (1,0), (0, -1), (-1, 0)]
        #print(f"Taking {step} steps facing {directions[facing]}")
        for _ in range(step):
            #next_y, next_x = v_add((y,x), deltas[facing])
            next_y = (y + deltas[facing][0]) % (max_y+1)
            next_x = (x + deltas[facing][1]) % (max_x+1)
            #print(f"        checking {next_y=} {next_x=}")
            while grid[next_y][next_x] == " ":
                next_y = (next_y + deltas[facing][0]) % (max_y+1)
                next_x = (next_x + deltas[facing][1]) % (max_x+1)
                #print(f"        checking {next_y=} {next_x=}")
            if grid[next_y][next_x] == "#":
                #print(f"Hit a wall at {next_y}, {next_x}")
                break
            y, x = next_y, next_x
            grid[y][x] = directions[facing]
            #print(f"Took a step, now at {y=} {x=}")
    #draw_grid(grid, max_y, max_x)
    return y, x, facing

def draw_grid(grid, max_y, max_x):
    s = ""
    for y in range(max_y+1):
        min_x = min(x for x in grid[y].keys() if grid[y][x] != " ")
        s += " "*min_x
        for x in range(max_x+1):
            #print(cave[y][x], end="")
            if grid[y][x] != " ":
                s += grid[y][x]
        s += "\n"
    print(s)

def parse_input(lines):
    grid = defaultdict(lambda: defaultdict(lambda: " "))
    path = []
    parse_map = True
    max_x, max_y = 0, 0
    for y, line in enumerate(lines):
        if line == "":
            parse_map = False
        if parse_map:
            for x, c in enumerate(line):
                if c in ".#":
                    max_x = max(max_x, x)
                    max_y = max(max_y, y)
                    grid[y][x] = c
        else:
            nr_tiles = 0
            for m in re.finditer(r"([0-9]+)([RL])?", line):
                tiles, turn = m.groups()
                if turn is None:
                    path += [int(tiles)]
                else:
                    path += [int(tiles), turn]

    return grid, path, max_y, max_x

def v_add(v1, v2):
    return tuple(v1[i]+v2[i] for i in range(len(v1)))

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            #input = (char for char in input.strip())
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))

