import os
import re
from collections import defaultdict

EXAMPLES = {
    "part1": {
        """\
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
""": 95437
    },
}

class Tree:
    def __init__(self, name="", size=0, children=None, parent=None):
        self._name = name
        self._size = size
        if children is None:
            children = []
        self._children = children
        self._parent = parent
        self._full_path = name if parent is None else parent._full_path.rstrip("/")+f"/{name}"

    def cd(self, name):
        return [c for c in self._children if c._name == name][0]

    def update_size(self):
        s = self._size
        for c in self._children:
            s += c.update_size()
        self._size = s
        return self._size

    def __repr__(self):
        return f"{self._full_path}: {self._size=}"
    
def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    return calc_size(lines)

def part2(lines):
    pass

def calc_size(lines):
    fs=None
    folders_flat = {}
    for line in lines:
        print(f"{line=} {fs=}")
        if m := re.match(r"\$ cd (.*)", line):
            name = m.group(1)
            if name == "/":
                fs = Tree(name="/")
            elif name == "..":
                fs = fs._parent
            else:
                fs = fs.cd(name)
        elif m := re.match(r"\$ ls", line):
            pass
        elif m := re.match(r"dir (.*)", line):
            name = m.group(1)
            child = Tree(name=name, parent=fs)
            fs._children += [child]
            folders_flat[child._full_path] = child
        elif m := re.match(r"([0-9]+) (.*)", line):
            size = int(m.group(1))
            name = m.group(2)
            child = Tree(name=name, parent=fs, size=size)
            fs._children += [child]
            folders_flat[child._full_path] = child

    while fs._parent is not None:
        fs = fs._parent

    fs.update_size()

    for f in folders_flat.values():
        if f._size <= 100000 and f._children != []:
            print(f)
    return sum(f._size for f in folders_flat.values() if f._size <= 100000 and f._children != [])

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
