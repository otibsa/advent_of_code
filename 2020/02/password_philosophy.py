import re
import inspect

TEST_INPUT = """1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"""

TEST_OUTPUT = "2"
TEST_OUTPUT_PART2 = "1"

def get_lines(func):
    def wrapped(s):
        return str(func([line for line in s.split("\n") if line != '']))
    return wrapped

@get_lines
def part1(lines):
    return count_valid_passwords(lines, is_valid_password)

@get_lines
def part2(lines):
    return count_valid_passwords(lines, is_valid_password2)

def count_valid_passwords(lines, cond):
    i = 0
    for line in lines:
        if cond(*parse_line(line)):
            i += 1
    return i

def parse_line(line):
    if m := re.match(r"(.*?)-(.*?) (.*?): (.*)", line):
        return int(m.group(1)), int(m.group(2)), m.group(3), m.group(4)

def is_valid_password(_min, _max, letter, password):
    return _min <= len([c for c in password if c == letter]) <= _max

def is_valid_password2(posA, posB, letter, password):
    if password[posA-1] == letter:
        return password[posB-1] != letter
    if password[posB-1] == letter:
        return password[posA-1] != letter
    return False

def test_part1():
    assert part1(TEST_INPUT) == TEST_OUTPUT

def test_part2():
    assert part2(TEST_INPUT) == TEST_OUTPUT_PART2

if __name__ == "__main__":
    with open("input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
