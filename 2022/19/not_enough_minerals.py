import os
import re
from collections import defaultdict
from math import inf
from pprint import pprint

EXAMPLES = {
    "part1": {
        """\
Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.
""": 33,
    """\
Blueprint 1: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 19 clay. Each geode robot costs 2 ore and 12 obsidian.
""": 1
    },
    "part2": {
        """\
Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.
""": 56*62,
    }
}

index = {"ore": 0, "clay": 1, "obsidian": 2, "geode": 3}
names = sorted(index, key=index.get)
type_tuples = {m: tuple(1 if i == index[m] else 0 for i in range(4)) for m in index.keys()}

g_playbook = {
    1: "clay,clay,clay,obsidian,clay,obsidian,geode,geode,geode".split(","),
    2: "ore,clay,ore,clay,clay,clay,clay,clay,clay,obsidian,clay,clay,obsidian,obsidian,clay,obsidian,geode,geode,geode".split(",")
}

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    blueprints = parse_input(lines)
    quality_level = 0
    for i, costs in blueprints.items():
        # print(f"Blueprint {i} costs: ")
        # for m in names:
        #     print(f"{m:>8}: {costs[m]}")

        geodes, path = collect_geodes(costs, 24)
        # print(f"Got {geodes} geodes using path {path}")
        # print()
        quality_level += i*geodes

    return quality_level

def part2(lines):
    blueprints = parse_input(lines)
    product = 1
    for i, costs in list(blueprints.items())[:3]:
        print(f"Blueprint {i} costs: ")
        for m in names:
            print(f"{m:>8}: {costs[m]}")

        geodes, path = collect_geodes(costs, 32)
        print(f"Got {geodes} geodes using path {path}")
        print()
        product *= geodes
    return product

def collect_geodes(costs, remaining_time, minerals=None, robots=None, most_geodes=0, path=None, playbook=None):
    if minerals is None:
        minerals = (0,0,0,0)
    if robots is None:
        robots = (1,0,0,0)
    if path is None:
        path = []

    # lots of off-by-one error patching going on here
    if remaining_time <= 1:
        # print(f"Out of time, {most_geodes=}  this path's geodes={minerals[index['geode']]} {','.join([p[1] for p in path])}")
        if remaining_time == 1:
            minerals = v_add(minerals, robots)
            remaining_time -= 1
        return minerals[index["geode"]], path

    # the potential score is how many geodes we could get if we had infinite
    # resources and would build one geode robot per time unit until the end
    potential_score = minerals[index["geode"]] + robots[index["geode"]]
    if remaining_time >= 1:
        r = robots[index["geode"]]
        for t in range(remaining_time):
            potential_score += r
            r += 1
    if potential_score <= most_geodes:
        return potential_score, path

    options = set(names)
    if robots[index["obsidian"]] == 0:
        options -= {"geode"}
    if robots[index["clay"]] == 0:
        options -= {"obsidian"}

    for m in set(names)-set(["geode"]):
        if robots[index[m]] >= max(costs[r][index[m]] for r in names):
            # no sense producing more minerals per time unit than we can spend
            # per time unit
            options -= {m}

    best_path = []
    if playbook is not None:
        options = [playbook[len(path)]]
    for target_robot in sorted(options, key=lambda m: index[m])[::-1]:
        tmp_minerals = tuple(minerals)
        tmp_remaining_time = remaining_time
        tmp_robots = tuple(robots)
        while any(tmp_minerals[i] < costs[target_robot][i] for i in range(4)) and tmp_remaining_time > 1:
            # wait until we have the minerals to build this robot
            tmp_minerals = v_add(tmp_minerals, robots)
            # print(f"T-{tmp_remaining_time:>2}:  waiting {target_robot:>8} robot r={tmp_robots} m={tmp_minerals}")
            tmp_remaining_time -= 1

        tmp_minerals = v_sub(tmp_minerals, costs[target_robot])
        tmp_minerals = v_add(tmp_minerals, tmp_robots)
        tmp_robots = v_add(robots, type_tuples[target_robot])

        # print(f"T-{tmp_remaining_time:>2}: building {target_robot:>8} robot r={tmp_robots} m={tmp_minerals}")
        score, tmp_path = collect_geodes(
                costs, tmp_remaining_time-1, minerals=tmp_minerals,
                robots=tmp_robots, most_geodes=most_geodes,
                path=path+[(tmp_remaining_time, target_robot)],
                playbook=playbook)

        if score > most_geodes:
            most_geodes = score
            best_path = tmp_path

    return most_geodes, best_path

def parse_input(lines):
    blueprints = {}
    for line in lines:
        if m := re.match(r"Blueprint ([0-9]+)", line):
            i = int(m.group(1))
            blueprints[i] = {}
        if m := re.match(r".*ore robot costs ([0-9]+) ore", line):
            blueprints[i]["ore"] = (int(m.group(1)), 0, 0, 0)
        if m := re.match(r".*clay robot costs ([0-9]+) ore", line):
            blueprints[i]["clay"] = (int(m.group(1)), 0, 0, 0)
        if m := re.match(r".*obsidian robot costs ([0-9]+) ore and ([0-9]+) clay", line):
            blueprints[i]["obsidian"] = (int(m.group(1)), int(m.group(2)), 0, 0)
        if m := re.match(r".*geode robot costs ([0-9]+) ore and ([0-9]+) obsidian", line):
            blueprints[i]["geode"] = (int(m.group(1)), 0, int(m.group(2)), 0)

    return blueprints

def v_add(v1, v2):
    return tuple(map(sum, zip(v1, v2)))

def v_sub(v1, v2):
    return tuple(map(lambda z: z[0]-z[1], zip(v1,v2)))

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
