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
TEST_OUTPUT_PART2 = "336"

def get_lines(func):
    def wrapped(s):
        return str(func([line for line in s.split("\n") if line != ""]))
    return wrapped

@get_lines
def part1(lines):
    return count_trees(lines)

@get_lines
def part2(lines):
    trees = 1
    for slope in [(1,1), (3,1), (5,1), (7,1), (1,2)]:
        trees *= count_trees(lines, step=slope[0], stepY=slope[1])
    return trees

def count_trees(lines, step=3, stepY=1):
    pos=0
    trees = 0
    for y, line in enumerate(lines):
        if y%stepY != 0:
            continue
        if line[pos%len(line)] == "#":
            trees += 1
        pos += step
    return trees

def test_part1():
    assert part1(TEST_INPUT) == TEST_OUTPUT

def test_part2():
    assert part2(TEST_INPUT) == TEST_OUTPUT_PART2

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
