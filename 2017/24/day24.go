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

func finish_bridge(port int, built_bridge []component, cs map[component]int, longest bool) []component {
	possible_bridges := [][]component{built_bridge}
	for c,n := range cs {
		if n > 0 && (c.portA == port || c.portB == port) {
			other_port := c.portB
			if c.portB == port {
				other_port = c.portA
			}
			cs[c]--
			b := finish_bridge(other_port, append(built_bridge, c), cs, longest)
			possible_bridges = append(possible_bridges,  b)
			cs[c]++
		}
	}

	best_strength, ix := get_strength(possible_bridges[0]), 0
	if longest {
		best_length := len(possible_bridges[0])
		for i,b := range possible_bridges {
			l := len(b)
			s := get_strength(b)
			if l > best_length || (l == best_length && s > best_strength) {
				best_length, best_strength, ix = l, s, i
			}
		}
	} else {
		for i,b := range possible_bridges {
			s := get_strength(b)
			if s > best_strength {
				best_strength, ix = s, i
			}
		}
	}
	best_bridge := make([]component, len(possible_bridges[ix]))
	copy(best_bridge, possible_bridges[ix])

	return best_bridge
}

func build_bridge(cs map[component]int, longest bool) int {
	bridge := finish_bridge(0, []component{}, cs, longest)
	strength := get_strength(bridge)
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

	strength := build_bridge(cs, false)
	strength_part2 := build_bridge(cs, true)

	return strconv.Itoa(strength), strconv.Itoa(strength_part2)
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("0/2\n2/2\n2/3\n3/5\n3/4\n0/1\n10/1\n9/10\n"), "31", "19"},
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
