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
""": 13},
    "part2": {
        """\
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
""": 1,
        """\
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
""": 36,
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
    visited = move_rope(lines, rope_len=10)
    return sum(sum(visited[y].values()) for y in visited.keys())

def print_grid(h, ts, visited, print_rope=True, print_visited=False):
    min_y, max_y = min(0, h[0], *visited.keys()), max(5, h[0], *visited.keys())
    min_x, max_x = min(0, h[1]), max(5, h[1])
    for y in range(min_y, max_y+1):
        min_x = min(min_x, min_x, *visited[y].keys())
        max_x = max(max_x, max_x, *visited[y].keys())

    range_y, range_x = range(min_y, max_y+1), range(min_x, max_x+1)

    for y in reversed(range_y):
        for x in range_x:
            c = "."
            if x == 0 and y == 0:
                c = "s"
            if print_visited and visited[y][x]:
                c = "#"
            for i,t in reversed(list(enumerate(ts))):
                if y == t[0] and x == t[1] and print_rope:
                    c = str(i+1)
            if y == h[0] and x == h[1] and print_rope:
                c = "H"

            print(c, end="")
        print()
    print()

def move_rope(lines, rope_len=2):
    visited = defaultdict(lambda: defaultdict(bool))
    deltas={
        "U": (1, 0),
        "D": (-1, 0),
        "R": (0, 1),
        "L": (0, -1),
    }
    rope = [[0,0] for _ in range(rope_len)]
    for line in lines:
        m = re.match(r"([UDRL]) ([0-9]+)", line)
        if not m:
            continue
        direction = m.group(1)
        distance = int(m.group(2))
        # go baby steps
        for step in range(distance):
            rope[0] = [rope[0][yx] + deltas[direction][yx] for yx in [0,1]]
            for i in range(1, len(rope)):
                d = [rope[i-1][yx] - rope[i][yx] for yx in [0,1]]
                for yx in [0,1]:
                    if abs(d[yx]) > 1:
                        if not abs(d[1-yx]) > 1:
                            # only move to the same row/column if it doesn't
                            # take two steps (special case for part 2)
                            rope[i][1-yx] = rope[i-1][1-yx]
                        rope[i][yx] += d[yx]//abs(d[yx])

            visited[rope[-1][0]][rope[-1][1]] = True
        #print_grid(rope[0], rope[1:], visited)

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
