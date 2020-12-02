TEST_INPUT = """1721
979
366
299
675
1456"""
TEST_OUTPUT = "514579"

def string_int_list(f):
    def wrapped(s):
        return str(f([int(i) for i in s.split()]))
    return wrapped

@string_int_list
def part1(xs):
    for i in range(len(xs)):
        if 2020-xs[i] in xs[i:]:
            return xs[i]*(2020-xs[i])
    raise Exception("no solution found")

def test_part1():
    assert part1(TEST_INPUT) == TEST_OUTPUT

def main():
    with open('input.txt') as f:
        print(part1(f.read()))

if __name__ == "__main__":
    main()
