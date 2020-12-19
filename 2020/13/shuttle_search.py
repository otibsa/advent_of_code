import sys
import os
import textwrap
import re
import math

EXAMPLES = {
    "part1": {
        """\
        939
        7,13,x,x,59,x,31,19
        """: 295
    },
    "part2": {
        """\
        0
        17,x,13,19
        """: 3417,
        """\
        0
        67,7,59,61
        """: 754018,
        """\
        0
        67,x,7,59,61
        """: 779210,
        """\
        0
        67,7,x,59,61
        """: 1261476,
        """\
        939
        7,13,x,x,59,x,31,19
        """: 1068781,
        """\
        0
        1789,37,47,1889
        """: 1202161486,
    }
}

def get_lines(func):
    def wrapped(*args, **kwargs):
        return str(func([line for line in args[0].split("\n") if line != ""], *args[1:], **kwargs))
    return wrapped

@get_lines
def part1(lines):
    timestamp = int(lines[0])
    bus_ids = lines[1].split(",")
    bus_ids = [int(i) for i in bus_ids if i != "x"]
    i, delay = find_bus(timestamp, bus_ids)
    return i*delay

@get_lines
def part2(lines):
    remainders = {}
    for diff, mod in enumerate(lines[1].split(",")):
        if mod == "x":
            continue
        mod = int(mod)
        remainders[mod] = -diff % mod
    return solve(remainders)

def find_bus(timestamp, bus_ids):
    next_arrivals = {i: i*math.ceil(timestamp/i) for i in bus_ids}
    next_bus = min(next_arrivals, key=next_arrivals.get)
    return next_bus, next_arrivals[next_bus]-timestamp

def solve_congruency(m1, r1, m2, r2):
    """ Find x such that x%m1 == r1 and x%m2 == r2 """
    # we have to use a generator comprehension, because a list comprehension
    # uses way too much memory
    for x in (r1+m1*i for i in range(m1*m2)):
        if x%m2 == r2:
            # print(f"x%{m1} == {r1} and x%{m2} == {r2}, {x=}")
            return x

def solve(offsets):
    """
    Two buses (e.g. 7, 13) are at the station at t=0 at the same time.
    The next time they align is at t=7*13=91 (if coprime, otherwise lcd(7,13)).

    That means that when they arrive with a time difference of 1, that also repeats
    every 91 minutes. So we can think of those two as a single virtual bus that
    starts at the first time where t mod 7 == 0 and t mod 13 == -1 (which is t=77)
    and has a travel time of 91.

    By applying this reduction step repeatedly, we can reduce the list of buses to a
    single virtual bus and the first arrival time of that bus is our solution.
    """
    sorted_travel_times = sorted(offsets.keys())[::-1]
    v_travel_time = sorted_travel_times[0]
    v_offset = offsets[v_travel_time]
    for travel_time in sorted_travel_times[1:]:
        offset = offsets[travel_time]
        # solve x%v_travel_time == v_offset and x%travel_time == offset
        v_offset = solve_congruency(v_travel_time, v_offset, travel_time, offset)
        v_travel_time *= travel_time
    return v_offset

def solve_slow(rs):
    sorted_moduli = sorted(rs.keys())[::-1]
    biggest_modulus = sorted_moduli[0]
    sorted_moduli = sorted_moduli[1:]
    x = rs[biggest_modulus]
    while True:
        if all(x%m == rs[m] for m in sorted_moduli):
            return x
        x += biggest_modulus

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
