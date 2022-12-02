import sys
import os
import textwrap
import re
import math

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

@get_lines
def part1(lines):
    rules, my_ticket, nearby_tickets = parse_input(lines)
    all_ranges = set()
    for ranges in rules.values():
        all_ranges |= set(ranges[0])
        all_ranges |= set(ranges[1])

    ticket_scanning_error_rate = 0
    for ticket in nearby_tickets:
        #print(f"Looking at ticket {ticket}")
        for value in ticket:
            if value not in all_ranges:
                #print(f"{value} not in any valid range")
                ticket_scanning_error_rate += value
            # for field_name, ranges in rules.items():
            #     if value in set(ranges[0])|set(ranges[1]):
            #         print(f"{value=} valid for field {field_name}")
            #         valid_fields += 1
            #         break
    return ticket_scanning_error_rate


@get_lines
def part2(lines):
    pass

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
