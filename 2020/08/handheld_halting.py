import os
import textwrap
import re

EXAMPLES = {
    "part1": {
        """\
        nop +0
        acc +1
        jmp +4
        acc +3
        jmp -3
        acc -99
        acc +1
        jmp -4
        acc +6
        """: 5
    },
}

def get_lines(func):
    def wrapped(s):
        return str(func([line for line in s.split("\n") if line != ""]))
    return wrapped

@get_lines
def part1(lines):
    state = {
        "pc": 0,
        "past": [],
        "program": lines,
        "acc": 0,
        "halted": False
    }
    state = run(state)
    return state["acc"]

def cmd_nop(arg, state):
    state["pc"] += 1
    return state

def cmd_acc(arg, state):
    state["acc"] += arg
    state["pc"] += 1
    return state

def cmd_jmp(arg, state):
    state["pc"] += arg
    return state

def run(state, debug=False):
    if debug:
        for i, line in enumerate(state["program"]):
            print(f"{i: 4}: {line}")
    while not state["halted"]:
        if state["pc"] < 0:
            state["halted"] = True
            state["error"] = "Negative pc"
            continue
        if state["pc"] in state["past"]:
            state["halted"] = True
            continue

        # fetch
        instruction = state["program"][state["pc"]]

        # decode
        cmd = lambda a, s: s.update({"halted": True, "error": f"Unknown opcode"})
        arg = 0
        if m := re.match(r"(.{3}) ([+-][0-9]+)", instruction):
            opcode = m.group(1)
            arg = int(m.group(2))
            if opcode == "nop":
                cmd = cmd_nop
            elif opcode == "acc":
                cmd = cmd_acc
            elif opcode == "jmp":
                cmd = cmd_jmp
        else:
            cmd = lambda a,s: s.update({"halted": True, "error": "Could not decode instruction"})

        # execute
        state["past"].append(state["pc"])
        if debug:
            print(f"pc={state['pc']: 4}, acc={state['acc']: 4}, cmd={cmd.__name__} {arg}, past={state['past']}")
        state = cmd(arg, state)

    if e := state.get("error") or debug:
        print(f"Program halted:")
        print(f"pc={state['pc']} acc={state['acc']} error={state.get('error')}, past={state['past']}")
        for i in range(state["pc"]-5, state["pc"]+5):
            if i < 0:
                continue
            print(f"{i: 5}: {state['program'][i]}")
    return state

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
