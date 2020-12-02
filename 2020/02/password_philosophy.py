import re
import inspect

TEST_INPUT = """1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"""

TEST_OUTPUT = "2"

def get_lines(func):
    def wrapped(s):
        return str(func([line for line in s.split("\n") if line != '']))
    return wrapped

@get_lines
def part1(lines):
    i = 0
    for line in lines:
        if is_valid_password(*parse_line(line)):
            i += 1
    return i

def parse_line(line):
    if m := re.match(r"(.*?)-(.*?) (.*?): (.*)", line):
        return int(m.group(1)), int(m.group(2)), m.group(3), m.group(4)

def is_valid_password(_min, _max, letter, password):
    return _min <= len([c for c in password if c == letter]) <= _max

def test_part1():
    assert part1(TEST_INPUT) == TEST_OUTPUT

if __name__ == "__main__":
    with open("input.txt") as f:
        print(part1(f.read()))
