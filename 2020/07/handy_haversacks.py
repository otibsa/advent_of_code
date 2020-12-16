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
    }
}

def get_lines(func):
    def wrapped(s):
        return str(func([line for line in s.split("\n") if line != ""]))
    return wrapped

@get_lines
def part1(lines):
    rules = parse_rules(lines)
    return len(traverse(start="shiny gold", dag=rules["is_inside"], target=set()))

def parse_rules(lines):
    contains = {}
    is_inside = {}
    for line in lines:
        if m := re.match(r"(.*) bags contain ([^.]+).", line):
            container = m.group(1)
            contains[container] = {}
            is_inside[container] = is_inside.get(container) or set()
            for m_inner in re.finditer(r"([0-9]+) ([^,]*) bags?", m.group(2)):
                count = m_inner.group(1)
                item = m_inner.group(2)
                #contains[container] = contains.get(container) or {}
                contains[container][item] = count
                is_inside[item] = is_inside.get(item) or set()
                is_inside[item] |= {container}
    return {"contains": contains, "is_inside": is_inside}

def traverse(start, dag, target):
    if dag.get(start) == target:
        return target
    result = set()
    for item in dag[start]:
        result |= {item}
        result |= traverse(item, dag, target)
    return result

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
