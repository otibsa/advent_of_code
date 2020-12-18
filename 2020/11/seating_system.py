import sys
import os
import textwrap
import re
import copy

EXAMPLES = {
    "part1": {
        """\
        L.LL.LL.LL
        LLLLLLL.LL
        L.L.L..L..
        LLLL.LL.LL
        L.LL.LL.LL
        L.LLLLL.LL
        ..L.L.....
        LLLLLLLLLL
        L.LLLLLL.L
        L.LLLLL.LL
        """: 37
    },
    "part2": {
        """\
        L.LL.LL.LL
        LLLLLLL.LL
        L.L.L..L..
        LLLL.LL.LL
        L.LL.LL.LL
        L.LLLLL.LL
        ..L.L.....
        LLLLLLLLLL
        L.LLLLLL.L
        L.LLLLL.LL
        """: 26
    },
}

def get_lines(func):
    def wrapped(*args, **kwargs):
        return str(func([line for line in args[0].split("\n") if line != ""], *args[1:], **kwargs))
    return wrapped

@get_lines
def part1(lines):
    seats = [list(line) for line in lines]
    rounds, new_seats = find_fixpoint(seats, apply_rules)
    return count_occupied(new_seats)

@get_lines
def part2(lines):
    seats = [list(line) for line in lines]
    rounds, new_seats = find_fixpoint(seats, lambda s: apply_rules(s, tolerance=5, distance_limit=None))
    return count_occupied(new_seats)

def count_occupied(seats):
    return len("".join("".join(s for s in row if s == "#") for row in seats))

def find_fixpoint(x, f):
    i = 1
    f_x = f(x)
    while f_x != x:
        x = f_x
        f_x = f(x)
        i += 1
    return i, f_x

def apply_rules(seats, tolerance=4, distance_limit=1):
    new_seats = copy.deepcopy(seats)
    for y in range(len(seats)):
        for x in range(len(seats[y])):
            n = count_neighbors(seats, x, y, distance_limit=distance_limit)
            if seats[y][x] == "L" and n == 0:
                new_seats[y][x] = "#"
            if seats[y][x] == "#" and n >= tolerance:
                new_seats[y][x] = "L"
    return new_seats

def count_neighbors(seats, x, y, distance_limit):
    directions = [(i,j) for i in [-1,0,1] for j in [-1,0,1] if i!=0 or j!=0]

    n = 0
    for d in directions:
        if find_neighbor(seats, x, y, d, limit=distance_limit):
            n += 1

    return n

def find_neighbor(seats, x, y, direction, limit):
    steps = 0
    n_x, n_y = x, y
    while limit is None or steps < limit:
        # step in the direction
        n_x += direction[0]
        n_y += direction[1]
        if n_y < 0 or n_y >= len(seats) or n_x < 0 or n_x >= len(seats[n_y]):
            # reached the wall, no neighbor
            return False
        seat = seats[n_y][n_x]
        steps += 1
        if seat == "#":
            # found one
            return True
        if seat == "L":
            # we found an empty seat, so no neighbor
            return False
    # reached the step limit, no neighbor
    return False

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
