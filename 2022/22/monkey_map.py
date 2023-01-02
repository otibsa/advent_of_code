import os
import re
from collections import defaultdict
from time import sleep, time
from pprint import pprint

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
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
""": 6032,
    },
    "part2": {
        ("""\
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
""", 4): 5031
    },
}

directions = ">v<^"

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines, **kwargs):
    grid, path, max_y, max_x = parse_input(lines)
    #draw_grid(grid, max_y, max_x)
    y = 0
    x = min(x for x in grid[y].keys() if grid[y][x] == ".")
    facing = 0
    y, x, facing = walk(grid, path, y, x, facing, max_y, max_x)
    return 1000*(y+1) + 4*(x+1) + facing

def part2(lines, edge=50):
    grid, path, max_y, max_x = parse_input(lines)
    #draw_grid(grid, max_y, max_x)
    y = 0
    x = min(x for x in grid[y].keys() if grid[y][x] == ".")
    facing = 0
    net = parse_grid(grid, max_y, max_x, edge)
    y, x, facing = walk_dice(grid, path, y, x, facing, net, edge)
    return 1000*(y+1) + 4*(x+1) + facing

def walk(grid, path, y, x, facing, max_y, max_x):
    grid[y][x] = directions[facing]
    for step in path:
        if type(step) == str:
            facing += 1 if step == "R" else -1
            facing = facing % 4
            grid[y][x] = directions[facing]
            continue
        deltas = [(0,1), (1,0), (0, -1), (-1, 0)]
        for _ in range(step):
            next_y = (y + deltas[facing][0]) % (max_y+1)
            next_x = (x + deltas[facing][1]) % (max_x+1)
            while grid[next_y][next_x] == " ":
                next_y = (next_y + deltas[facing][0]) % (max_y+1)
                next_x = (next_x + deltas[facing][1]) % (max_x+1)
            if grid[next_y][next_x] == "#":
                break
            y, x = next_y, next_x
            grid[y][x] = directions[facing]
    #draw_grid(grid, max_y, max_x)
    return y, x, facing

def walk_dice(grid, path, y, x, facing, net, edge=50):
    grid[y][x] = directions[facing]
    for step in path:
        if type(step) == str:
            facing += 1 if step == "R" else -1
            facing %= 4
            grid[y][x] = directions[facing]
            continue
        deltas = [(0,1), (1,0), (0, -1), (-1, 0)]
        for _ in range(step):
            row, col = y//edge, x//edge
            from_left = {
                0: y%edge,
                1: (-x-1)%edge,
                2: (-y-1)%edge,
                3: x%edge
            }[facing]

            if ((x%edge == 0 and facing == 2)
                    or (y%edge == 0 and facing == 3)
                    or (x%edge == edge-1 and facing == 0)
                    or (y%edge == edge-1 and facing == 1)):
                next_row, next_col, next_facing = net[(row, col)][facing]
                if next_facing == 0:
                    next_y = next_row*edge + from_left
                    next_x = next_col*edge
                elif next_facing == 1:
                    next_y = next_row*edge
                    next_x = (next_col+1)*edge - from_left - 1
                elif next_facing == 2:
                    next_y = (next_row+1)*edge - from_left - 1
                    next_x = (next_col+1)*edge - 1
                elif next_facing == 3:
                    next_y = (next_row+1)*edge - 1
                    next_x = next_col*edge + from_left
                #print(f"Crossing an edge at {y,x} facing {facing} ({row=}, {col=}), next: {next_row, next_col} facing {next_facing} {from_left=}")
            else:
                next_y = y + deltas[facing][0]
                next_x = x + deltas[facing][1]
                next_facing = facing
            if grid[next_y][next_x] == "#":
                # print(f"Hit a wall at {next_y}, {next_x}")
                break
            y, x, facing = next_y, next_x, next_facing
            grid[y][x] = directions[facing]
    # draw_grid(grid, max_y, max_x)
    return y, x, facing

def draw_grid(grid, max_y, max_x):
    s = "   "
    for x in range(max_x+1):
        if x%10 == 0:
            s += f"{x//10:2}"
        elif x%10 == 9:
            pass
        else:
            s += " "
    s += "\n    "
    for x in range(max_x+1):
        s += str(x%10)
    s += "\n"
    for y in range(max_y+1):
        s += f"{y:3} "
        min_x = min(x for x in grid[y].keys() if grid[y][x] != " ")
        s += " "*min_x
        for x in range(max_x+1):
            #print(cave[y][x], end="")
            if grid[y][x] != " ":
                s += grid[y][x]
        s += "\n"
    print(s)

def parse_grid(grid, max_y, max_x, edge=50):
    prev_row, prev_col = None, None
    net = dict()
    for row in range((max_y//edge)+1):
        for col in range((max_x//edge)+1):
            if grid[row*edge][col*edge] != " ":
                net[(row, col)] = dict()
                # get direct neighbors
                for direction, d in enumerate([(0,1), (1,0), (0,-1), (-1,0)]):
                    if grid[(row+d[0])*edge][(col+d[1])*edge] != " ":
                        net[(row, col)][direction] = (row+d[0], col+d[1], direction)
    for _ in range(3):
        # we probably don't have to do more than 3 folds
        for row, col in net.keys():
            for d in range(4):
                if d in net[row,col]:
                    # only work on free edges
                    continue
                # check if we already have a clockwise neighbor (d+1)
                if neighbor := net[row,col].get((d+1)%4):
                    # neighbor is already connected to us
                    nrow, ncol, ndir = neighbor
                    # check the counterclockwise neighbor of that neighbor
                    # (ingress direction - 1)
                    if dst := net[nrow,ncol].get((ndir-1)%4):
                        # dst is connected to neighbor
                        drow, dcol, ddir = dst
                        # the relevant egress direction of dst is
                        # counterclockwise again
                        if net[drow,dcol].get((ddir-1)%4) is None:
                            # dst has a free edge in the correct direction, so
                            # let's connect
                            net[row,col][d] = (drow, dcol, (ddir+1)%4)
                            net[drow,dcol][(ddir-1)%4] = (row, col, (d+2)%4)

    return net

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
                #face = net.split("\n")[y//edge][x//edge]
                if c in ".#":
                    max_x = max(max_x, x)
                    max_y = max(max_y, y)
                    grid[y][x] = c if c == "#" else "."   #face
        else:
            nr_tiles = 0
            for m in re.finditer(r"([0-9]+)([RL])?", line):
                tiles, turn = m.groups()
                if turn is None:
                    path += [int(tiles)]
                else:
                    path += [int(tiles), turn]

    return grid, path, max_y, max_x

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            edge=None
            if part == "part2":
                input, edge = input
            input = (line for line in input.split("\n"))
            output = globals()[part](input, edge=edge)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))

