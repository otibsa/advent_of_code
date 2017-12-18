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

const frequency_reg = 0
const recovered_frequency_reg = 1

func do(line []string, registers map[byte]int, pc *int) {
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

	// fmt.Printf("%02v: %v, regX: %v, x: %v, regY: %v, y: %v, registers: %v\n", *pc, line, regX, x, regY, y, registers)
	*pc++
	switch line[0] {
	case "snd":
		//fmt.Printf("(snd %v), registers: %v\n", frequency_reg, registers)
		registers[frequency_reg] = x

	case "set":
		registers[regX] = y

	case "add":
		registers[regX] += y

	case "mul":
		registers[regX] *= y

	case "mod":
		registers[regX] = registers[regX] % y

	case "rcv":
		if x != 0 {
			registers[recovered_frequency_reg] = registers[frequency_reg]
			// don't want to run forever
			*pc = -1
		}

	case "jgz":
		if x > 0 {
			*pc-- //revert previous increment
			*pc += y
		}
	}
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
	for pc<len(instructions) && pc != -1 {
		do(instructions[pc], registers, &pc)
	}
	return strconv.Itoa(registers[recovered_frequency_reg]), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader(`set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2`), "4", ""},
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
