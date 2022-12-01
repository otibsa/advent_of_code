import os

EXAMPLES = {
    "part1": {
        """\
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
""": 24000
    }
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    max_calories = 0
    elf_sum = 0
    for line in lines:
        # print(f"{max_calories=} {elf_sum=} {line=}")
        if line == "":
            max_calories = max(elf_sum, max_calories)
            elf_sum = 0
        else:
            elf_sum += int(line)
    return max_calories

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for input, output in EXAMPLES["part1"].items():
        assert part1(input.split("\n")) == output, f"got {part1(input)}, should have been {output}"
    print(part1(line_generator(filename)))
