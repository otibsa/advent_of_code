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
    "part2": {
        """\
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw""": 70
    }
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def rucksack_generator(line_gen):
    for r in line_gen:
        yield (r[:len(r)//2], r[len(r)//2:])

def group_generator(line_gen):
    for r1 in line_gen:
        r2=next(line_gen)
        r3=next(line_gen)
        yield (r1, r2, r3)

def part1(lines):
    return find_common(rucksack_generator(lines))

def part2(lines):
    return find_common(group_generator(lines))

def find_common(tuple_gen):
    sum_prio = 0
    for compartments in tuple_gen:
        common_items = set.intersection(*map(set, compartments))
        for item in common_items:
            offset = ord("A")-27 if "A" <= item <= "Z" else ord("a")-1
            sum_prio += ord(item)-offset
    return sum_prio

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
