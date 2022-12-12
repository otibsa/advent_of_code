import os
import re
from collections import defaultdict
from math import inf

EXAMPLES = {
    "part1": {
        """\
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
""": 31
    },
    "part2": {
        """\
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
""": 29
    }
}

class Graph:
    def __init__(self):
        self.edges = dict()

    def __repr__(self):
        return f"{self.edges}"

    def add_edge(self, src, dst, weight=1):
        if not src in self.edges:
            self.edges[src] = dict()
        if not dst in self.edges:
            self.edges[dst] = dict()
        self.edges[src][dst] = weight

    def adjust_weights(self, func):
        for src, adjacents in list(self.edges.items()):
            for dst, weight in list(adjacents.items()):
                self.edges[src][dst] = func(weight)
                if self.edges[src][dst] == inf:
                    del self.edges[src][dst]


    def find_shortest_path(self, start, end, heuristic, debug=False):
        # A* algorithm
        came_from = dict()
        g_score = defaultdict(lambda: inf)
        g_score[start] = 0
        f_score = defaultdict(lambda: inf)
        f_score[start] = heuristic(start)
        fringe = set([start])
        while fringe:
            current = min(list(fringe), key=lambda node: f_score[node])
            if current == end:
                return came_from
            fringe.discard(current)
            for neighbor, weight in self.edges[current].items():
                tentative_score = g_score[current] + weight
                if tentative_score < g_score[neighbor]:
                    came_from[neighbor] = current
                    g_score[neighbor] = tentative_score
                    f_score[neighbor] = tentative_score + heuristic(neighbor)
                    fringe.add(neighbor)
            if debug:
                tmp = sorted([(f_score[node], node) for node in list(fringe)])
                print(f"{current=} {tmp=}")

def line_generator(filename):
    with open(filename) as f:
        while line := f.readline():
            yield line.strip("\n")

def part1(lines):
    graph, start, end = parse_input(lines)
    # edge weight zero messes up the path finding, we are only interested in
    # number of steps for now

    # remove steps taller than 1 and replace all resulting weights with
    # weight 1
    graph.adjust_weights(lambda w: 1 if w <= 1 else inf)
    def vertical_distance(node):
        return ord(end[2]) - ord(node[2])
    def manhattan_distance(node):
        return abs(end[0]-node[0])+abs(end[1]-node[1])
    def dijkstra(node):
        return 0
    came_from = graph.find_shortest_path(start, end, heuristic=manhattan_distance)
    path = unfold_path(end, came_from, draw=False)
    # path includes the list of visited nodes including start and end (so the
    # path length is len(path)-1)
    return len(path)-1

def part2(lines):
    graph, start, end = parse_input(lines)
    # walking in reverse, at most one step down
    graph.adjust_weights(lambda w: 1 if w >= -1 else inf)
    #for src in graph.edges.keys():
    #    print(f"{src}: {graph.edges[src]}")

    def dijkstra(node):
        return 0
    # start at the top, go to the start
    going_to = graph.find_shortest_path(end, start, heuristic=dijkstra)
    # using heuristic=0 devolves A* into Dijkstra which generates the shortest
    # paths to all nodes from a single starting point.
    # This means all "a" nodes were visited and are part of going_to
    possible_starts = [node for node in going_to.keys() if node[2] == "a"]
    shortest_path = min((unfold_path(ps, going_to) for ps in possible_starts), key=lambda path: len(path))
    return len(shortest_path)-1

def unfold_path(end, came_from, draw=False):
    ys = [node[0] for node in came_from.keys()]
    xs = [node[1] for node in came_from.keys()]
    dimensions = (1+max(ys)-min(ys), 1+max(xs)-min(xs))
    chars=[["." for cols in range(dimensions[1])] for rows in range(dimensions[0])]
    chars[end[0]][end[1]] = "E"
    path = [end]
    node = end
    prev = came_from[node]
    while prev:
        if prev[1] < node[1]:
            # node is to the right of prev
            c = ">"
        elif prev[0] < node[0]:
            c = "v"
        elif prev[1] > node[1]:
            c = "<"
        elif prev[0] > node[0]:
            c = "^"
        chars[prev[0]][prev[1]] = c
        node = prev
        prev = came_from.get(node)
        path = [node] + path
    if draw:
        for line in chars:
            for c in line:
                print(c, end="")
            print()
    return path

def parse_input(lines):
    lines = list(lines)
    char_matrix = []
    for line in lines:
        if line != "":
            char_matrix += [list(line)]
    graph = Graph()
    start = (0,0,"S")
    end = (0,0,"E")
    for y, row in enumerate(char_matrix):
        for x, c in enumerate(row):
            if c == "S":
                c = "a"
                start = (y,x,c)
            if c == "E":
                c = "z"
                end = (y,x,c)
            deltas = [(0,1),(0,-1),(1,0),(-1,0)]
            for d in deltas:
                tmp_y, tmp_x = y+d[0], x+d[1]
                if 0 <= tmp_y < len(char_matrix) and 0 <= tmp_x < len(row):
                    tmp_c = char_matrix[tmp_y][tmp_x]
                    if tmp_c == "S":
                        tmp_c = "a"
                    if tmp_c == "E":
                        tmp_c = "z"
                    graph.add_edge((y,x,c), (tmp_y, tmp_x, tmp_c), ord(tmp_c) - ord(c))
    return graph, start, end

if __name__ == "__main__":
    filename = os.path.dirname(os.path.realpath(__file__))+"/input.txt"
    for part, example_dict in EXAMPLES.items():
        for input, expected_output in example_dict.items():
            input = (line for line in input.split("\n"))
            output = globals()[part](input)
            assert output == expected_output, f"{part}(...)={output}, should have been {expected_output}"
    print(part1(line_generator(filename)))
    print(part2(line_generator(filename)))
