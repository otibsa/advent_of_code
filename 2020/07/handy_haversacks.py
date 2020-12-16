import os
import textwrap
import re

EXAMPLES = {
    "part1": {
        """\
        light red bags contain 1 bright white bag, 2 muted yellow bags.
        dark orange bags contain 3 bright white bags, 4 muted yellow bags.
        bright white bags contain 1 shiny gold bag.
        muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
        shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
        dark olive bags contain 3 faded blue bags, 4 dotted black bags.
        vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
        faded blue bags contain no other bags.
        dotted black bags contain no other bags.""": 4
    },
    "part2": {
        """\
        light red bags contain 1 bright white bag, 2 muted yellow bags.
        dark orange bags contain 3 bright white bags, 4 muted yellow bags.
        bright white bags contain 1 shiny gold bag.
        muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
        shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
        dark olive bags contain 3 faded blue bags, 4 dotted black bags.
        vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
        faded blue bags contain no other bags.
        dotted black bags contain no other bags.""": 32,
        """\
        shiny gold bags contain 2 dark red bags.
        dark red bags contain 2 dark orange bags.
        dark orange bags contain 2 dark yellow bags.
        dark yellow bags contain 2 dark green bags.
        dark green bags contain 2 dark blue bags.
        dark blue bags contain 2 dark violet bags.
        dark violet bags contain no other bags.""": 126
    }
}

def get_lines(func):
    def wrapped(s):
        return str(func([line for line in s.split("\n") if line != ""]))
    return wrapped

@get_lines
def part1(lines):
    rules = parse_rules(lines)
    t = traverse(start="shiny gold", dag=rules["is_inside"])
    # print(t)
    return len(t.keys())

@get_lines
def part2(lines):
    rules = parse_rules(lines)
    t = traverse(start="shiny gold", dag=rules["contains"])
    # print(t)
    return sum(t.values())

def parse_rules(lines):
    contains = {}
    is_inside = {}
    for line in lines:
        if m := re.match(r"(.*) bags contain ([^.]+).", line):
            container = m.group(1)
            contains[container] = {}
            is_inside[container] = is_inside.get(container) or {}
            for m_inner in re.finditer(r"([0-9]+) ([^,]*) bags?", m.group(2)):
                count = int(m_inner.group(1))
                item = m_inner.group(2)
                contains[container][item] = count
                is_inside[item] = is_inside.get(item) or {}
                is_inside[item][container] = 0
    return {"contains": contains, "is_inside": is_inside}

def traverse(start, dag):
    if dag.get(start) == {}:
        return {}
    result = {}
    for item, count in dag[start].items():
        result[item] = result.get(item) or 0
        result[item] += count
        for subitem, subcount in traverse(item, dag).items():
            result[subitem] = result.get(subitem) or 0
            result[subitem] += count*subcount
    return result

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
