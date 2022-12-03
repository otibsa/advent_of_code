import os

EXAMPLES = {
    "part1": {
        """\
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw""": 157
    },
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    return find_common(lines)

def part2(lines):
    pass

def find_common(rucksacks):
    sum_prio = 0
    for rucksack in rucksacks:
        left, right = rucksack[:len(rucksack)//2], rucksack[len(rucksack)//2:]
        common_items = set(left)&set(right)
        for item in common_items:
            offset = ord("A")-27 if "A" <= item <= "Z" else ord("a")-1
            sum_prio += ord(item)-offset
    return sum_prio

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            output = globals()[part](input.split("\n"))
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
