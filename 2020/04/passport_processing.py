import os
import re
import pprint

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
TEST_INPUT_PART2 = [
    """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
    hcl:#623a2f

    eyr:2029 ecl:blu cid:129 byr:1989
    iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

    hcl:#888785
    hgt:164cm byr:2001 iyr:2015 cid:88
    pid:545766238 ecl:hzl
    eyr:2022

    iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719""",
    """eyr:1972 cid:100
    hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

    iyr:2019
    hcl:#602927 eyr:1967 hgt:170cm
    ecl:grn pid:012533040 byr:1946

    hcl:dab227 iyr:2012
    ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

    hgt:59cm ecl:zzz
    eyr:2038 hcl:74454a iyr:2023
    pid:3556412378 byr:2007"""
]
TEST_OUTPUT_PART2 = ["4", "0"]

def get_sections(func):
    def wrapped(s):
        return str(func([section for section in re.split("\n\n+", s) if section != ""]))
    return wrapped

@get_sections
def part1(sections):
    return count_valid_passports(sections)

@get_sections
def part2(sections):
    return count_valid_passports(sections, is_valid_passport2)

def is_valid_passport(**fields):
    return all(key in fields.keys() for key in ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"])

class E(Exception):
    pass

def is_valid_passport2(**fields):
    byr = fields.get("byr")
    iyr = fields.get("iyr")
    eyr = fields.get("eyr")
    hgt = fields.get("hgt")
    hcl = fields.get("hcl")
    ecl = fields.get("ecl")
    pid = fields.get("pid")
    if any(f is None for f in [byr,iyr,eyr,hgt,hcl,ecl,pid]):
        raise E(f"Missing field in {sorted(fields.keys())}")

    if len(byr) != 4 or int(byr) < 1920 or 2002 < int(byr):
        raise E(f"Bad byr: {byr}")

    if len(iyr) != 4 or int(iyr) < 2010 or 2020 < int(iyr):
        raise E(f"Bad iyr: {iyr}")

    if len(eyr) != 4 or int(eyr) < 2020 or 2030 < int(eyr):
        raise E(f"Bad eyr: {eyr}")

    if hgt[-2:] == "in":
        if int(hgt[:-2]) < 59 or 76 < int(hgt[:-2]):
            raise E(f"Bad hgt(in): {hgt}")
    elif hgt[-2:] == "cm":
        if int(hgt[:-2]) < 150 or 193 < int(hgt[:-2]):
            raise E(f"Bad hgt(cm): {hgt}")
    else:
        raise E(f"Bad hgt: {hgt}")

    if not re.match(r"#[0-9a-f]{6}$", hcl):
        raise E(f"Bad hcl: {hcl}")

    if ecl not in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]:
        raise E(f"Bad ecl: {ecl}")

    if not re.match(r"[0-9]{9}$", pid):
        raise E(f"Bad pid: {pid}")

    if 'cid' in fields:
        del fields['cid']
    return True

def count_valid_passports(sections, cond=is_valid_passport):
    n = 0
    for passport in sections:
        fields = [f.split(":") for f in re.split(" |\n", passport)]
        fields = dict(f for f in fields if len(f) == 2)
        try:
            if cond(**fields):
                # print(f"  VALID: {pprint.pformat(fields,compact=True,width=1000)}")
                n += 1
        except E as e:
            # print(f"INVALID: {pprint.pformat(fields,compact=True,width=1000)}, {e}")
            pass
    return n
        
def test_part1():
    assert part1(TEST_INPUT) == TEST_OUTPUT

def test_part2():
    for i in range(2):
        assert part2(TEST_INPUT_PART2[i]) == TEST_OUTPUT_PART2[i]

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
