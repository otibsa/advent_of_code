#!/usr/bin/env python

import sys

def main():
    # read directions from user input,
    # print number of visited coordinates
    houses = visit({0:{0:1}}, (0, 0))
    return len(houses)

def visit(houses, pos):
    c = sys.stdin.read(1)
    x,y = pos
    if c == "<":
        x,y = left(pos)
    elif c == ">":
        x,y = right(pos)
    elif c == "^":
        x,y = up(pos)
    elif c == "v":
        x,y = down(pos)
    else:
        visit(houses, pos)

    if x not in houses:
        houses[x] = {}
    houses[x][y] = houses[x].get(y, 0) + 1  # increase number of visits
    print houses
    visit(houses, (x,y))


def up((x,y)):
    return (x,y+1)

def down((x,y)):
    return (x,y-1)

def left((x,y)):
    return (x-1, y)

def right((x,y)):
    return (x+1, y)
    

if __name__ == "__main__":
    main()
