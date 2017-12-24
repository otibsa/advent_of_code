package main

import (
	"fmt"
	"io"
	"os"
	"strings"
	"bufio"
	"strconv"
)

type component struct {
	portA, portB int
}

type test_input struct {
	input    io.Reader
	output_1 string
	output_2 string
}

func get_strength(bridge []component) int {
	strength := 0
	for _,c := range bridge {
		strength += c.portA + c.portB
	}
	return strength
}

func finish_bridge(port int, built_bridge []component, cs map[component]int) []component {

	possible_bridges := [][]component{built_bridge}
	// fmt.Printf("\n\nfinish_bridge(%v, %v, %v)\n", port, built_bridge, cs)
	for c,n := range cs {
		if n > 0 && (c.portA == port || c.portB == port) {
			// fmt.Printf("c = %v\n", c)
			other_port := c.portB
			if c.portB == port {
				other_port = c.portA
			}
			cs[c]--
			b := finish_bridge(other_port, append(built_bridge, c), cs)
			// fmt.Printf("b = %v\n", b)
			if len(b) > 0 {
				possible_bridges = append(possible_bridges,  b)
			}
			cs[c]++
		}
	}

	best_strength, ix := get_strength(possible_bridges[0]), 0
	for i,b := range possible_bridges {
		s := get_strength(b)
		if s > best_strength {
			best_strength, ix = s, i
		}
	}
	return possible_bridges[ix]

}

func build_bridge(cs map[component]int) int {
	bridge := finish_bridge(0, []component{}, cs)
	strength := get_strength(bridge)
	// fmt.Printf("strongest bridge: %v (%v)\n", bridge, strength)
	return strength
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)

	cs := map[component]int{}
	for scanner.Scan() {
		line := strings.Split(strings.TrimSpace(scanner.Text()), "/")
		if len(line) != 2 {
			continue
		}
		a, err := strconv.Atoi(line[0])
		if err != nil {
			continue
		}
		b, err := strconv.Atoi(line[1])
		if err != nil {
			continue
		}
		cs[component{portA: a, portB: b}]++
	}

	strength := build_bridge(cs)
	return strconv.Itoa(strength), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10\n"), "31", ""},
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
