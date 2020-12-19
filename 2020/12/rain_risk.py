import os
import textwrap
import re
import copy

EXAMPLES = {
    "part1": {
        """\
        F10
        N3
        F7
        R90
        F11
        """: 25
    },
    "part2": {
        """\
        F10
        N3
        F7
        R90
        F11
        """: 286
    },
}

def get_lines(func):
    def wrapped(s):
        return str(func([line for line in s.split("\n") if line != ""]))
    return wrapped

@get_lines
def part1(lines):
    state = {
        "x": 0,
        "y": 0,
        "heading": 90
    }
    navigate(state, instructions=lines)
    return manhattan_distance(0,0, state["x"], state["y"])

@get_lines
def part2(lines):
    state = {
        "x": 0,
        "y": 0,
        "waypoint": {
            "x": 10,
            "y": 1
        }
    }
    navigate(state, instructions=lines, debug=False)
    return manhattan_distance(0,0, state["x"], state["y"])

def manhattan_distance(x1, y1, x2, y2):
    return abs(x2-x1)+abs(y2-y1)

def cmd_r(arg, state):
    arg = (arg+360)%360
    if "waypoint" in state:
        if arg == 90:
            state["waypoint"]["x"], state["waypoint"]["y"] = state["waypoint"]["y"], -state["waypoint"]["x"]
        if arg == 180:
            state["waypoint"]["x"], state["waypoint"]["y"] = -state["waypoint"]["x"], -state["waypoint"]["y"]
        if arg == 270:
            state["waypoint"]["x"], state["waypoint"]["y"] = -state["waypoint"]["y"], state["waypoint"]["x"]
        return
    state["heading"] = (state["heading"] + arg) % 360

def cmd_l(arg, state):
    cmd_r(-arg, state)

def cmd_f(arg, state, heading=None):
    if "waypoint" in state:
        state["x"] += arg*state["waypoint"]["x"]
        state["y"] += arg*state["waypoint"]["y"]
        return
    if heading is None:
        heading = state["heading"]
    if heading == 0:
        state["y"] += arg
    if heading == 90:
        state["x"] += arg
    if heading == 180:
        state["y"] -= arg
    if heading == 270:
        state["x"] -= arg

def cmd_n(arg, state):
    if "waypoint" in state:
        state["waypoint"]["y"] += arg
        return
    cmd_f(arg,state,heading=0)

def cmd_e(arg, state):
    if "waypoint" in state:
        state["waypoint"]["x"] += arg
        return
    cmd_f(arg,state,heading=90)

def cmd_s(arg, state):
    if "waypoint" in state:
        cmd_n(-arg, state)
        return
    cmd_f(arg,state,heading=180)

def cmd_w(arg, state):
    if "waypoint" in state:
        cmd_e(-arg, state)
        return
    cmd_f(arg,state,heading=270)

def navigate(state, instructions, debug=False):
    if debug:
        for i, line in enumerate(instructions):
            print(f"{i: 4}: {line}")
    for instruction in instructions:
        # decode
        arg = 0
        if m := re.match(r"(.)([0-9]+)", instruction):
            opcode = m.group(1)
            arg = int(m.group(2))
            if opcode == "N":
                cmd = cmd_n
            elif opcode == "E":
                cmd = cmd_e
            elif opcode == "S":
                cmd = cmd_s
            elif opcode == "W":
                cmd = cmd_w
            elif opcode == "R":
                cmd = cmd_r
            elif opcode == "L":
                cmd = cmd_l
            elif opcode == "F":
                cmd = cmd_f
        else:
            cmd = lambda a,s: None

        # execute
        if debug:
            if "waypoint" in state:
                print(f"x={state['x']: 4}, y={state['y']: 4}, waypoint={state['waypoint']}, {instruction}")
            else:
                print(f"x={state['x']: 4}, y={state['y']: 4}, heading={state['heading']:03}, {instruction}")
        cmd(arg, state)

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
        print(part2(input))
