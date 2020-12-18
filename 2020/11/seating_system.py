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

def apply_rules(seats):
    new_seats = copy.deepcopy(seats)
    for y in range(len(seats)):
        for x in range(len(seats[y])):
            n = count_neighbors(seats, x, y)
            if seats[y][x] == "L" and n == 0:
                new_seats[y][x] = "#"
            if seats[y][x] == "#" and n >= 4:
                new_seats[y][x] = "L"
    return new_seats

def count_neighbors(seats, x, y):
    n = 0
    if y>0:
        if x>0 and seats[y-1][x-1] == "#":
            n += 1
        if seats[y-1][x] == "#":
            n += 1
        if x+1<len(seats[y-1]) and seats[y-1][x+1] == "#":
            n += 1

    if x>0 and seats[y][x-1] == "#":
        n += 1
    if x+1<len(seats[y]) and seats[y][x+1] == "#":
        n += 1

    if y+1 < len(seats):
        if x>0 and seats[y+1][x-1] == "#":
            n += 1
        if seats[y+1][x] == "#":
            n += 1
        if x+1<len(seats[y+1]) and seats[y+1][x+1] == "#":
            n += 1
    return n

@get_lines
def part2(lines):
    pass

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
