import os

EXAMPLES = {
    "part1": {
        "BFFFBBFRRR": 567,
        "FFFBBBFRRR": 119,
        "BBFFBBFRLL": 820,
        """BFFFBBFRRR
        FFFBBBFRRR
        BBFFBBFRLL""": 820
    }
}

def get_lines(func):
    def wrapped(s):
        return str(func([line for line in s.split("\n") if line != ""]))
    return wrapped

@get_lines
def part1(lines):
    return max(decode_seat(bsp) for bsp in lines)

@get_lines
def part2(lines):
    seat_ids = [decode_seat(bsp) for bsp in lines]
    return find_gap(seat_ids)

def decode_seat(bsp):
    """
    The Binary Seat Partition is a binary number with 'F'/'L' instead of '0' and
    'B'/'R' instead of '1'.
    """
    seat_id = 0
    for c in bsp:
        seat_id <<= 1
        if c in ["F", "L"]:
            pass
        elif c in ["B", "R"]:
            seat_id += 1
    return seat_id
    # alternative with O(4*n):
    # return int(bsp.replace("F","0").replace("L","0").replace("B","1").replace("R","1"), 2)

def find_gap(xs):
    """
    Preconditions: There is only a single number missing in the list, it's not
    the first and not the last
    """
    xs = sorted(xs)
    offset = xs[0]
    for i, x in enumerate(xs):
        if x-i > offset:
            return x-1

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](input) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
