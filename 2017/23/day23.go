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

func do(line []string, registers map[byte]int, pc *int, mul_ctr *int) {
	regX := []byte(line[1])[0]
	x, err := strconv.Atoi(line[1])
	if err != nil {
		x = registers[regX]
	}

	var regY byte = 0
	var y int = 0
	if len(line) > 2 {
		regY = []byte(line[2])[0]
		y, err = strconv.Atoi(line[2])
		if err != nil {
			y = registers[regY]
		}
	}

	*pc++
	switch line[0] {
	case "set":
		registers[regX] = y

	case "sub":
		registers[regX] -= y

	case "mul":
		registers[regX] *= y
		if mul_ctr != nil {
			*mul_ctr++
		}

	case "jnz":
		if x != 0 {
			*pc-- //revert previous increment
			*pc += y
		}
	}
}

func part2(x, y int) int {
	counter := 0
	for x <= y {
		is_prime := true
		for d:=2; d<x; d++ {
			for e:=2; e<x; e++ {
				if d*e == x {
					is_prime = false
				}
			}
		}
		if !is_prime {
			counter++
		}
		x += 17
	}
	return counter
}

func part2_shorter(x, y int) int {
	counter := 0
	for x <= y {
		is_prime := true
		for d:=2; d<x/2; d++ {
			if x % d == 0 {
				is_prime = false
				break
			}
		}
		if !is_prime {
			counter++
		}
		x += 17
	}
	return counter
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)

	registers := map[byte]int{}
	instructions := [][]string{}
	for scanner.Scan() {
		line := strings.Fields(strings.TrimSpace(scanner.Text()))
		instructions = append(instructions, line)
	}

	pc := 0
	mul_ctr := 0
	for pc<len(instructions) && pc != -1 {
		do(instructions[pc], registers, &pc, &mul_ctr)
	}

	// part 2
	non_prime_counter := part2_shorter(105700, 122700)

	return strconv.Itoa(mul_ctr), strconv.Itoa(non_prime_counter)
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{}
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
