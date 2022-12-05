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
    },
    "part2": {
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
""": 45000
    }
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def _part1(lines):
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

def elf_generator(lines):
    elf = []
    for line in lines:
        if line == "":
            yield elf
            elf = []
        else:
            elf += [line]


def heavy_carriers(lines):
    calories = [sum(int(line) for line in elf) for elf in elf_generator(lines)]
    # calories = []
    # for elf in elf_generator(lines):
    #     elf_sum = 0
    #     for line in elf:
    #         elf_sum += int(line)
    #     calories += [elf_sum]
    return sorted(calories, reverse=True)

def part1(lines):
    return heavy_carriers(lines)[0]

def part2(lines):
    return sum(heavy_carriers(lines)[:3])

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            #assert part1(input.split("\n")) == output, f"got {_part1(input)}, should have been {output}"
            print(f"{input=} {expected_output=}")
            output = globals()[part](input.split("\n"))
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
