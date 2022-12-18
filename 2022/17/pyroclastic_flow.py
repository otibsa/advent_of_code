import os
import re
from collections import defaultdict
from time import sleep, time

EXAMPLES = {
    "part1": {
        """\
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
""": 3068,
    },
    "part2": {
        """\
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
""": 1514285714288,
    },
}

rocks = [
    "@@@@",
    ".@.\n@@@\n.@.",
    "@@@\n..@\n..@",
    "@\n@\n@\n@",
    "@@\n@@"
]

def char_generator(filename):
    with open(filename) as f:
        while c := f.read(1):
            if c not in "<>":
                continue
            yield c

def part1(chars):
    jets = list(chars)
    grid = defaultdict(lambda: ["." for _ in range(7)])
    grid = drop_rocks(grid, jets, 2022)
    #draw_grid(grid)
    return get_height(grid)

def part2(chars):
    jets = list(chars)
    grid = defaultdict(lambda: ["." for _ in range(7)])
    grid = drop_rocks(grid, jets, 1000000000000)
    return get_height(grid)

def drop_rocks(grid, jets, nr_rocks):
    max_column_height = [-1 for _ in range(7)]
    hashes = {}
    skipped = 0
    i = 0
    j = 0
    while i < nr_rocks:
        h = hash((i%len(rocks), j%len(jets), tuple(["".join(line) for line in grid.values()])))
        height = get_height(grid)
        if h not in hashes:
            hashes[h] = (i, j, height)
        elif not skipped:
            prev_i, prev_j, prev_height = hashes[h]
            skipped = nr_rocks // (i-prev_i) - 1
            if skipped <= 0:
                skipped = 1
            i = prev_i + skipped * (i-prev_i)
            j = prev_j + skipped * (j-prev_j)
            for y in list(grid.keys()):
                tmp = grid[y]
                del grid[y]
                grid[y+(skipped-1)*(height-prev_height)] = tmp
        rock = rocks[i%len(rocks)].split("\n")
        falling = True

        ry = height+3
        rx = 2

        #print("A new rock begins falling")
        #draw_grid(grid, rock, ry, rx)
        #print()
        while falling and ry >= 0:
            jet = 1 if jets[j%len(jets)] == ">" else -1
            pushed = True
            for y in range(len(rock)):
                for x in range(len(rock[y])):
                    if rock[y][x] == ".":
                        continue
                    if rx+x+jet < 0 or 7 <= rx+x+jet:
                        pushed = False
                        continue
                    if grid[ry+y][rx+x+jet] != ".":
                        pushed = False

            #print(f"Jet of gas pushes rock {'right' if jet>0 else 'left'}", end="")
            if pushed:
                #print()
                rx += jet
            else:
                #print(", but nothing happens")
                pass
            #draw_grid(grid, rock, ry, rx)
            #print()
            if ry <= 0:
                falling = False
            for y in range(len(rock)):
                for x in range(len(rock[y])):
                    if rock[y][x] == ".":
                        continue
                    if grid[ry+y-1][rx+x] != ".":
                        #print(f"rock hit another rock: {ry=} {rx=} {y=} {x=}")
                        falling = False
            j += 1
            if falling:
                #print("Rock falls 1 unit:")
                ry -= 1
                #draw_grid(grid, rock, ry, rx)
                #print()
            else:
                #print("Rock falls 1 unit, causing it to come to rest:")
                pass
        #print(f"Rock {rock} stopped falling at {ry=} {rx=}")
        for y in range(len(rock)):
            for x in range(len(rock[y])):
                if rock[y][x] != ".":
                    grid[ry+y][rx+x] = "#"
                    max_column_height[rx+x] = max(max_column_height[rx+x], ry+y)
        for y in list(grid.keys()):
            if y < min(max_column_height):
                # remove all covered layers
                del grid[y]
        i += 1

    return grid

def get_height(grid):
    if len(grid) == 0:
        return 0
    m = max(grid)  # max index doesn't mean there's a rock there
    for height in range(m+1)[::-1]:
        if "#" in grid[height]:
            return height+1
    return 0

def draw_grid(grid, rock=None, ry=None, rx=None):
    s = ""
    h = get_height(grid)
    if rock is not None:
        h = max(h, ry+len(rock))
    y = 0
    for y in sorted(grid.keys())[::-1]:
        if y<0 or y > h:
            continue
        s += f"{y:13} "
        s += "|"
        for x in range(7):
            if rock is not None:
                if ry <= y < ry+len(rock) and rx <= x < rx+len(rock[y-ry]) and rock[y-ry][x-rx] != ".":
                    s += rock[y-ry][x-rx]
                else:
                    s += grid[y][x]
            else:
                s += grid[y][x]
        s += "|"
        s += "\n"
    if y > 0:
        s += " "*13 + " %" + " "*7 + "%\n"
    s += " "*13 + " +"
    s += "-"*7
    s += "+"
    print(s)

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            #input = (line for line in input.split("\n"))
            input = (char for char in input.strip())
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(char_generator(filename)))
    print(part2(char_generator(filename)))
