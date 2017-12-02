#!/usr/bin/env python
input = "^>v<"
coords = [0,0]
coord_list = ["0,0"]
for x in input:
    if x == ">":
        coords[0]+=1
    if x == "<": 
        coords[0]-=1
    if x == "^":
        coords[1]+=1
    if x == "v":
        coords[1]-=1
    coord_list.append(str(coords[0])+","+str(coords[1]))

print set(coord_list)
