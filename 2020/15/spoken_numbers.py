import sys
import os
import textwrap
import re
import math
import time

EXAMPLES = {
    "part1": {
        "0,3,6": 436,
        "1,3,2": 1,
        "2,1,3": 10,
        "1,2,3": 27,
        "2,3,1": 78,
        "3,2,1": 438,
        "3,1,2": 1836
    },
}

class Number:
    def __init__(self, i):
        self.number = i
        s

def get_lines(func):
    def wrapped(*args, **kwargs):
        return str(func([line for line in args[0].split("\n") if line != ""], *args[1:], **kwargs))
    return wrapped

@get_lines
def part1(lines):
    starting_numbers = [int(n) for n in lines[0].split(",")]
    #return speak(starting_numbers)[2020]
    return speak(starting_numbers, 2020)

@get_lines
def part2(lines):
    starting_numbers = [int(n) for n in lines[0].split(",")]
    pass

def test_speak():
    print(speak([0,3,6], 5))

def speak(starting_numbers, nr_turns, spoken_numbers=None):
    # spoken_numbers[0] is the newest one!
    if nr_turns <= 0:
        return spoken_numbers[0]
    if spoken_numbers is None:
        spoken_numbers = []
    if starting_numbers:
        n = starting_numbers[0]
    else:
        # start the heavy lifting here
        last_spoken = spoken_numbers[0]
        #print(f"{last_spoken=} {spoken_numbers=}")
        if last_spoken not in spoken_numbers[1:]:
            # the previously spoken number is new
            n = 0
        else:
            # get age
            n = 1+spoken_numbers[1:].index(last_spoken)
    #time.sleep(1)
    #print(f"{n=} {starting_numbers=} {spoken_numbers=} {nr_turns=}")
    return speak(starting_numbers[1:], nr_turns-1, [n]+spoken_numbers)

# def speak(starting_numbers, spoken_numbers=None):
#     if starting_numbers:
#         yield starting_numbers[0]
#     yield from speak(starting_numbers[1:], spoken_numbers+[
# 
#     last = spoken_numbers[-1]
#     if last not in spoken_numbers[:-1]:
#         yield 0
#     else:
#         yield 

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            output = globals()[func_name](textwrap.dedent(input))
            assert output == str(expected_output), f"got {output}, expected {expected_output}"

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
