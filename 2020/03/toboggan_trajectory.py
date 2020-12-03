import os

TEST_INPUT = """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"""

TEST_OUTPUT = "7"

def get_lines(func):
    def wrapped(s):
        return str(func([line for line in s.split("\n") if line != ""]))
    return wrapped

@get_lines
def part1(lines):
    return count_trees(lines)

def count_trees(lines):
    pos=0
    step=3
    trees = 0
    for line in lines:
        if line[pos%len(line)] == "#":
            trees += 1
        pos += step
    return trees

def test_part1():
    assert part1(TEST_INPUT) == TEST_OUTPUT

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        print(part1(f.read()))
