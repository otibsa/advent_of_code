import os
import re
from collections import defaultdict
from time import sleep

EXAMPLES = {
    "part1": {
        """\
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
""": 3068,
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
    pass

def drop_rocks(grid, jets, nr_rocks):
    j = 0
    for i in range(nr_rocks):
        height = get_height(grid)
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
        #for y in range(len(rock)):
        #    grid[ry+y] = rx*"." + rock[y].replace("@", "#") + (7-rx)*"."
        for y in range(len(rock)):
            for x in range(len(rock[y])):
                if rock[y][x] != ".":
                    grid[ry+y][rx+x] = "#"
        #draw_grid(grid)
        #print()
        #print(get_height(grid))

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
    for y in range(h)[::-1]:
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
    s += "+"
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
