import os
import re
from collections import defaultdict

EXAMPLES = {
    "part1": {
        """\
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
""": 13
    },
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    visited = move_rope(lines)
    return sum(sum(visited[y].values()) for y in visited.keys())

def part2(lines):
    pass

def get_size(visited, minimum=5):
    min_y, max_y = min(0,0, *visited.keys()), max(5,5, *visited.keys())
    min_x, max_x = 0, 5
    for y in range(min_y, max_y+1):
        min_x = min(min_x, min_x, *visited[y].keys())
        max_x = max(max_x, max_x, *visited[y].keys())
    return range(min_y, max_y+1), range(min_x, max_x+1)

def print_grid(h, t, visited, print_ends=True):
    range_y, range_x = get_size(visited, 5)

    for y in reversed(range_y):
        for x in range_x:
            if y == h[0] and x == h[1] and print_ends:
                c = "H"
            elif y == t[0] and x == t[1] and print_ends:
                c = "T"
            elif visited[y][x]:
                c = "#"
            else:
                c = "."
            print(c, end="")
        print()
    print()

def move_rope(lines):
    visited = defaultdict(lambda: defaultdict(bool))
    deltas={
        "U": (1, 0),
        "D": (-1, 0),
        "R": (0, 1),
        "L": (0, -1),
    }
    h = [0, 0]
    t = [0, 0]
    for line in lines:
        m = re.match(r"([UDRL]) ([0-9]+)", line)
        if not m:
            continue
        direction = m.group(1)
        distance = int(m.group(2))
        # go baby steps
        for _ in range(distance):
            h = [h[yx] + deltas[direction][yx] for yx in [0,1]]
            for yx in [0,1]:
                d = h[yx] - t[yx]
                if abs(d) > 1:
                    t[1-yx] = h[1-yx]    # make sure we're not diagonal anymore
                    t[yx] += d//abs(d)   # -1 or +1

            visited[t[0]][t[1]] = True

    return visited

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
