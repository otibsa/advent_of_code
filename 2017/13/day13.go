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

type layer struct {
	_range int
	scanner int
}

func printFirewall(fw map[int]*layer) {
	fmt.Printf("[")
	for d,l := range fw {
		fmt.Printf("%v:{r: %v, s: %v}, ", d, l._range, l.scanner)
	}
	fmt.Printf("]\n")
}

func process(r io.Reader) (string, string) {
	scnr := bufio.NewScanner(r)
	firewall := map[int]*layer{}
	max_depth := 0
	for scnr.Scan() {
		line := strings.Split(scnr.Text(), ": ")
		depth, err := strconv.Atoi(line[0]) 
		if err != nil {
			continue
		}
		_range, err := strconv.Atoi(line[1])
		if err != nil {
			continue
		}
		firewall[depth] = &layer{_range:_range, scanner:0}
		if depth > max_depth {
			max_depth = depth
		}
	}

	severity := 0
	d := -1
	for d <= max_depth {
		// move packet
		d++

		// if packet == scanners[d]: bad
		l, ok := firewall[d]
		if ok && l.scanner == 0 && l._range > 0 {
			severity += d*l._range
			// fmt.Printf("caught in depth %v, range %v\n", d, l._range)
		}

		// move scanners
		for i, l := range firewall {
			firewall[i].scanner = (l.scanner+1) % (2*l._range-2) // down, then up
		}
	}

	// fmt.Printf("severity: %v\n", severity)
	return strconv.Itoa(severity), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("0: 3\n1: 2\n4: 4\n6: 4\n"), "24", ""},
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
