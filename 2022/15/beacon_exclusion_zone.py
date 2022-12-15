import os
import re
from collections import defaultdict
from time import sleep

EXAMPLES = {
    "part1": {
        """\
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
""": 26,
    },
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines, line_y=2000000):
    grid, closest_beacon, range_y, range_x = parse_input(lines)
    return len(find_blocked(closest_beacon, line_y))

def part2(lines):
    pass

def find_blocked(closest_beacon, line_y):
    beacon_list = set([(b[0], b[1]) for b in closest_beacon.values()])
    blocked_xs = set()
    for s in closest_beacon.keys():
        b_y, b_x, beacon_distance = closest_beacon[s]
        s_y, s_x = s[0], s[1]
        sensor_distance_to_line = line_y - s_y
        if beacon_distance < abs(sensor_distance_to_line):
            # this sensor can't reach the line
            continue
        x_reach = beacon_distance - abs(sensor_distance_to_line)
        blocked_xs |= set(range(s_x - x_reach, s_x + x_reach + 1))
        blocked_xs -= set(b[1] for b in beacon_list if b[0] == line_y)

    #draw_grid(grid, range_y, range(-10,30))
    return blocked_xs

def parse_input(lines):
    grid = defaultdict(lambda: defaultdict(lambda: "."))
    closest_beacon = dict()
    min_y, max_y = 0, 0
    min_x, max_x = 0, 0
    for line in lines:
        if m := re.match(r"Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)", line):
            s_x, s_y, b_x, b_y = map(int, m.groups())
            min_x = min(min_x, s_x, b_x)
            max_x = max(max_x, s_x, b_x)
            min_y = min(min_y, s_y, b_y)
            max_y = max(max_y, s_y, b_y)
            grid[s_y][s_x] = "S"
            grid[b_y][b_x] = "B"
            closest_beacon[(s_y, s_x)] = (b_y, b_x, abs(s_y-b_y)+abs(s_x-b_x))

    return grid, closest_beacon, range(min_y, max_y+1), range(min_x, max_x+1)

def draw_grid(grid, range_y, range_x):
    s = ""
    for y in range_y:
        for x in range_x:
            s += grid[y][x]
        s += "\n"
    print(s)

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input, line_y=10)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
