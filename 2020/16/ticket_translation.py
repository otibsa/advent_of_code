import sys
import os
import textwrap
import re
import math
from collections import defaultdict

EXAMPLES = {
    "part1": {
        """\
        class: 1-3 or 5-7
        row: 6-11 or 33-44
        seat: 13-40 or 45-50

        your ticket:
        7,1,14

        nearby tickets:
        7,3,47
        40,4,50
        55,2,20
        38,6,12
        """: 71
    },
    "part2": {
        """\
        class: 0-1 or 4-19
        row: 0-5 or 8-19
        seat: 0-13 or 16-19

        your ticket:
        11,12,13

        nearby tickets:
        3,9,18
        15,1,5
        5,14,9
        """: -1
    }
}

def get_lines(func):
    def wrapped(*args, **kwargs):
        return str(func([line for line in args[0].split("\n") if line != ""], *args[1:], **kwargs))
    return wrapped

def parse_input(lines):
    rules = {}
    lines = iter(lines)
    for line in lines:
        if m := re.match(r"(.*): (.*)-(.*) or (.*)-(.*)", line):
            field_name, ll, lh, rl, rh = m.group(1,2,3,4,5)
            rules[field_name] = (range(int(ll), int(lh)+1), range(int(rl), int(rh)+1))
        elif re.match(r"your ticket:", line):
            line = next(lines)
            my_ticket = list(map(int, line.split(",")))
        elif re.match(r"nearby tickets:", line):
            nearby_tickets = []
            for ticket in lines:
                nearby_tickets += [list(map(int, ticket.split(",")))]

    return rules, my_ticket, nearby_tickets


def find_valid_tickets(rules, nearby_tickets):
    all_ranges = set()
    for ranges in rules.values():
        all_ranges |= set(ranges[0])
        all_ranges |= set(ranges[1])

    ticket_scanning_error_rate = 0
    valid_tickets = []
    for ticket in nearby_tickets:
        error = 0
        for value in ticket:
            if value not in all_ranges:
                error += value
        ticket_scanning_error_rate += error
        if error == 0:
            valid_tickets += [ticket]

    return ticket_scanning_error_rate, valid_tickets

def find_fields(rules, my_ticket, valid_tickets):
    possible_field_index = {f: set(range(len(rules.keys()))) for f in rules.keys()}
    while any(len(indices)>1 for indices in possible_field_index.values()):
        for field_name in sorted(possible_field_index.keys(), key=lambda f: len(possible_field_index[f])):
            indices = possible_field_index[field_name]
            if len(indices) == 1:
                continue
            fields_left = True
            ranges = set(rules[field_name][0])|set(rules[field_name][1])
            for index in indices.copy():
                if not all(ticket[index] in ranges for ticket in valid_tickets):
                    indices -= {index}
            possible_field_index[field_name] = indices

        for field1 in possible_field_index.keys():
            indices1 = possible_field_index[field1]
            if len(indices1) == 1:
                for field2 in possible_field_index.keys():
                    if field2 == field1:
                        continue
                    possible_field_index[field2] -= indices1

    product = 1
    for field_name, index in possible_field_index.items():
        index = list(index)[0]
        if field_name.startswith("departure"):
            product *= my_ticket[index]
    return product

@get_lines
def part1(lines):
    rules, my_ticket, nearby_tickets = parse_input(lines)
    return find_valid_tickets(rules, nearby_tickets)[0]

@get_lines
def part2(lines):
    rules, my_ticket, nearby_tickets = parse_input(lines)
    _, valid_tickets = find_valid_tickets(rules, nearby_tickets)
    return find_fields(rules, my_ticket, valid_tickets)

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
