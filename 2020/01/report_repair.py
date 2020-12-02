TEST_INPUT = """1721
979
366
299
675
1456"""
TEST_OUTPUT_PART1 = "514579"
TEST_OUTPUT_PART2 = "241861950"

def string_int_list(f):
    def wrapped(s):
        return str(f([int(i) for i in s.split()]))
    return wrapped

@string_int_list
def part1(xs):
    return _part1(xs)

@string_int_list
def part2(xs):
    return _part2(xs)

def _part1(xs, summed=2020):
    for i in range(len(xs)):
        if summed-xs[i] in xs[i:]:
            return xs[i]*(summed-xs[i])
    raise Exception("no solution found")

def _part2(xs):
    for i in range(len(xs)):
        try:
            prod2 = _part1(xs[i:], summed=2020-xs[i])
            return xs[i] * prod2
        except:
            pass

def test_part1():
    assert part1(TEST_INPUT) == TEST_OUTPUT_PART1

def test_part2():
    assert part2(TEST_INPUT) == TEST_OUTPUT_PART2

def main():
    with open('input.txt') as f:
        input = f.read()
        print(part1(input))
        print(part2(input))

if __name__ == "__main__":
    main()
