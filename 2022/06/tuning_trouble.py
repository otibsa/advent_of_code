import os
import re
from collections import defaultdict

EXAMPLES = {
    "part1": {
        "mjqjpqmgbljsphdztnvjfqwrcgsmlb": 7,
        "bvwbjplbgvbhsrlpgdmjqwftvncz": 5,
        "nppdvjthqldpwncqszvftbrmjlhg": 6,
        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg": 10,
        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw": 11
    },
    "part2": {
        "mjqjpqmgbljsphdztnvjfqwrcgsmlb": 19,
        "bvwbjplbgvbhsrlpgdmjqwftvncz": 23,
        "nppdvjthqldpwncqszvftbrmjlhg": 23,
        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg": 29,
        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw": 26,
    }
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    return find_marker(next(lines))

def part2(lines):
    return find_marker(next(lines), marker_len=14)

def find_marker(packet, marker_len=4):
    for i, c in enumerate(packet[(marker_len-1):], marker_len-1):
        marker = packet[(i-marker_len+1):(i+1)]
        if len(set(marker)) == marker_len:
            break
    return i+1

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
