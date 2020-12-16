import os
import textwrap
import re

EXAMPLES = {
    "part1": {
        """\
        abc

        a
        b
        c

        ab
        ac

        a
        a
        a
        a

        b""": 11
    }
}

def get_lines(func):
    def wrapped(s):
        return str(func([line for line in s.split("\n") if line != ""]))
    return wrapped

def get_sections(func):
    def wrapped(s):
        return str(func([section for section in re.split("\n\n+", s) if section != ""]))
    return wrapped

@get_sections
def part1(sections):
    return count_answers(sections)

def count_answers(sections):
    answer_count = 0
    for group in sections:
        answers = set()
        for person in group.split("\n"):
            for answer in person:
                answers.add(answer)
        answer_count += len(answers)
    return answer_count

def test_examples():
    for func_name, example_dict in EXAMPLES.items():
        for input, output in example_dict.items():
            assert globals()[func_name](textwrap.dedent(input)) == str(output)

if __name__ == "__main__":
    with open(os.path.dirname(os.path.realpath(__file__))+"/input.txt") as f:
        input = f.read()
        print(part1(input))
