package main

import (
	"fmt"
	"io"
	"os"
	"strings"
	"bufio"
	"strconv"
)

type test_input struct {
	input    io.Reader
	output_1 string
	output_2 string
}

type position struct {
	x,y int
}

type carrier struct {
	pos position
	heading int
}

type field map[position]bool

func (f field) p(start position, current position, size int) {
	for y:=0; y<size; y++ {
		for x:=0; x<size; x++ {
			n := "."
			if f[position{x:x+start.x, y:y+start.y}] {
				n = "#"
			}
			if current.x == x+start.x && current.y == y+start.y {
				fmt.Printf("[%s]", n)
			} else {
				fmt.Printf(" %s ", n)
			}
		}
		fmt.Println()
	}
}

func process(r io.Reader) (string, string) {
	// read field from input
	scanner := bufio.NewScanner(r)
	f := field{}
	y := 0
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		for x:=0; x<len(line); x++ {
			if line[x] == byte('#') {
				f[position{x:x, y:y}] = true
			}
		}
		y++
	}

	// start at middle, heading north
	c := carrier{heading:0, pos:position{x:y/2, y:y/2}}
	infection_counter := 0
	for burst:=1; burst<=10000; burst++{
		turn := 1
		// turn right or left
		if !f[c.pos] {
			infection_counter++
			turn = -1
		}
		c.heading = (c.heading+turn+4) % 4
		// invert status of current node
		f[c.pos] = !f[c.pos]

		// go one step
		switch c.heading {
		case 0:
			c.pos.y--
		case 1:
			c.pos.x++
		case 2:
			c.pos.y++
		case 3:
			c.pos.x--
		}
	}

	return strconv.Itoa(infection_counter), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("..#\n#..\n...\n"), "5587", ""},
	}
	for _, t := range tests {
		sol_1, sol_2 := process(t.input)
		if t.output_1 != "" && t.output_1 != sol_1 {
			fmt.Printf("Part 1 test input \"%v\" failed: got \"%v\", wanted \"%v\"\n", t.input, sol_1, t.output_1)
		}
		if t.output_2 != "" && t.output_2 != sol_2 {
			fmt.Printf("Part 2 test input \"%v\" failed: got \"%v\", wanted \"%v\"\n", t.input, sol_2, t.output_2)
		}
	}

	part1, part2 := process(input)
	fmt.Println("Solution for part 1: \"" + part1 + "\"")
	fmt.Println("Solution for part 2: \"" + part2 + "\"")
}

func check_err(e error) {
	if e != nil {
		panic(e)
	}
}
