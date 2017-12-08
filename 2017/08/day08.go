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

func max_map(xs map[string]int) (max_key string, max_value int) {
	i := 0
	for k, v := range xs {
		if (i == 0) || (v > max_value) {
			max_key, max_value = k, v
		}
		i++
	}

	return max_key, max_value
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)
	registers := map[string]int{}
	max_value := 0
	for scanner.Scan() {
		line := strings.Fields(scanner.Text())
		if len(line) < 7 {
			fmt.Printf("Too few elements in line: \"%v\"\n", line)
			continue
		}
		reg := line[0]
		op := func(x, y int) int { return x }
		switch line[1] {
			case "inc":
				op = func(x, y int) int { return x+y }
			case "dec":
				op = func(x, y int) int { return x-y }
			default:
		}
		diff, err := strconv.Atoi(line[2])
		if err != nil {
			continue
		}
		cond_reg := line[4]
		cond_value, err := strconv.Atoi(line[6])
		if err != nil {
			continue
		}
		cond := func(x, y int) bool { return true }
		switch line[5] {
			case ">":
				cond = func(x, y int) bool { return x > y }
			case "<":
				cond = func(x, y int) bool { return x < y }
			case ">=":
				cond = func(x, y int) bool { return x >= y }
			case "<=":
				cond = func(x, y int) bool { return x <= y }
			case "==":
				cond = func(x, y int) bool { return x == y }
			case "!=":
				cond = func(x, y int) bool { return x != y }
		}

		// fmt.Printf("line: %v %v %v IF %v %v %v\n", reg, line[1], diff, cond_reg, line[5], cond_value)

		// apply line
		if cond(registers[cond_reg], cond_value) {
			registers[reg] = op(registers[reg], diff)
		}

		_, mv := max_map(registers)
		if mv > max_value {
			max_value = mv
		}
	}
	_, max_value_end := max_map(registers)
	return strconv.Itoa(max_value_end), strconv.Itoa(max_value)
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"), "1", "10"},
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
