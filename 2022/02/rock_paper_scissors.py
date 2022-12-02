import os

EXAMPLES = {
    "part1": {
        """\
A Y
B X
C Z""": 15
    }
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    score = 0
    for line in lines:
        if line == "":
            continue
        col1, col2 = line.strip().split(" ")
        col1 = ord(col1) - ord("A")
        col2 = ord(col2) - ord("X")

        score += col2+1

        if (col2-col1)%3 == 1:
            score += 6
        elif col2 == col1:
            score += 3
    return score

def part2(lines):
    pass

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            output = globals()[part](input.split("\n"))
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
