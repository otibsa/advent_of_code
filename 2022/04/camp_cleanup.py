import os
import re

EXAMPLES = {
    "part1": {
        """\
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8""": 2
    },
    "part2": {
        """\
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8""": 4
    }
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    return find_subsets(lines)[0]

def part2(lines):
    return find_subsets(lines)[1]

def find_subsets(lines):
    nr_subsets = 0
    nr_overlaps = 0
    for assignment in lines:
        if m := re.match(r"(.*)-(.*),(.*)-(.*)", assignment):
            ll,lh,rl,rh = map(int, m.groups())
            if (ll<=rl<=lh and ll<=rh<=lh) or (rl<=ll<=rh and rl<=lh<=rh):
                nr_subsets += 1
            if ll<=rl<=lh or ll<=rh<=lh or rl<=ll<=rh or rl<=lh<=rh:
                nr_overlaps += 1

    return nr_subsets, nr_overlaps

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
