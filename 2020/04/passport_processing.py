import os
import re

TEST_INPUT = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"""

TEST_OUTPUT = "2"

def get_sections(func):
    def wrapped(s):
        return str(func([section for section in re.split("\n\n+", s) if section != ""]))
    return wrapped

@get_sections
def part1(sections):
    return count_valid_passports(sections)

def count_valid_passports(sections):
    n = 0
    for passport in sections:
        fields = [f.split(":") for f in re.split(" |\n", passport)]
        fields = dict(f for f in fields if len(f) == 2)
        if is_valid_passport(fields):
            n += 1
    return n
        
def is_valid_passport(fields):
    return all(key in fields.keys() for key in ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"])

def test_part1():
    assert part1(TEST_INPUT) == TEST_OUTPUT

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
