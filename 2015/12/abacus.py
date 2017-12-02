#!/usr/bin/env python

import json
import pprint

def main():
    fp = open("input", "r")
    data = json.load(fp)
    pprint.pprint(data)
    print("The value is {0}".format(calc(data)))

def calc(thing):
    # pprint.pprint(thing)
    t = type(thing)
    if t is list:
        return sum(map(calc, thing))
    if t is dict:
        if "red" in thing.values():
            return 0
        return sum(map(calc, thing.values()))
    if t is int:
        return thing
    return 0

if __name__ == "__main__":
    main()
