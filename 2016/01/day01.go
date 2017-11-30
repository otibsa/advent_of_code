package main

import (
	"fmt"
	"io/ioutil"
	"strings"
	"strconv"
	"math"
)


func walk(input string) (string, string) {
	heading := 0 // 0 == North
	pos_x, pos_y := 0, 0
	found_intersection := false
	intersection_distance := 0
	visited := make(map[string]bool) // using strings as the index is probably not a good idea, better would be a tuple?
	input = strings.Trim(input, "\n")
	for _, c := range strings.Split(input, ",") {
		c = strings.Trim(c, " ")
		// e.g. c = "R2"
		// parse turn "R"
		//fmt.Printf("step: %-5v ", c)
		turn := c[0]
		if turn == 'L' {
			heading = (4 + heading - 1) % 4
		} else if turn == 'R' {
			heading = (4 + heading + 1) % 4
		} else {
			continue
		}

		// parse "2"
		blocks, err := strconv.Atoi(c[1:])
		if err != nil {
			continue
		}

		for i:=0; i<blocks; i++ {
			// go blocks
			switch heading {
			case 0:
				//North
				pos_y++
			case 1:
				//East
				pos_x++
			case 2:
				//South
				pos_y--
			case 3:
				//West
				pos_x--
			}
			if !found_intersection && visited[fmt.Sprintf("%v,%v", pos_x, pos_y)] {
				found_intersection = true
				intersection_distance = int(math.Abs(float64(pos_x)) + math.Abs(float64(pos_y)))
			} else {
				visited[fmt.Sprintf("%v,%v", pos_x, pos_y)] = true
			}

		}
		//fmt.Printf("new position: %3v, %3v\theading: %v\tdistance: %v\n", pos_x, pos_y, heading, math.Abs(float64(pos_x)) + math.Abs(float64(pos_y)))
	}

	distance := math.Abs(float64(pos_x)) + math.Abs(float64(pos_y))
	solution1 := fmt.Sprintf("%v", distance)
	solution2 := "\"don't know\""
	if found_intersection {
		solution2 = fmt.Sprintf("%v", intersection_distance)
	}
	return solution1, solution2
}

func main() {
	input, err := ioutil.ReadFile("input.txt")  // it would be more efficient to only iterate over the content once
	check_err(err)

	if sol, _ := walk("R2, L3"); sol != "5" {
		panic(sol+" should be 5 (part1)")
	}
	if sol, _ := walk("R2,R2,R2"); sol != "2" {
		panic(sol+" should be 2 (part1)")
	}
	if sol, _ := walk("R5, L5, R5, R3"); sol != "12" {
		panic(sol+" should be 12 (part1)")
	}
	if _, sol := walk("R8,R4,R4,R8"); sol != "4" {
		panic(sol+" should be 4 (part2)")
	}
	fmt.Println("Test input succeeded")
	sol_part1, sol_part2 := walk(string(input))
	fmt.Printf("Taxicab distance between start and end point: %v\n", sol_part1)
	fmt.Printf("First path intersection: %v\n", sol_part2)
}

func check_err(e error) {
	if e != nil {
		panic(e)
	}
}
