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

func max(xs []int) (int,int) {
	if len(xs) == 0 {
		return -1, 0 
	}
	max_index, max_value := 0, xs[0]
	for i, x := range xs {
		if x > max_value {
			max_index, max_value = i, x
		}
	}
	return max_index, max_value
}

func process(r io.Reader) (string, string) {
	// read banks from input.txt
	scanner := bufio.NewScanner(r)
	banks := []int{}
	if scanner.Scan() {
		for _, s := range strings.Split(scanner.Text(), "\t") {
			if x, err := strconv.Atoi(s); err == nil {
				banks = append(banks, x)
			}
		}
	}
	if len(banks) == 0 {
		return "", ""
	}

	// cycle through reallocations
	seen := map[string]bool{}
	var cycle int
	for cycle = 0; !seen[fmt.Sprintf("%v", banks)]; cycle++ {
		seen[fmt.Sprintf("%v", banks)] = true
		// fmt.Printf("% 3v: %v\n", cycle, banks)

		index, blocks := max(banks)
		banks[index] = 0
		for blocks > 0 {
			index++
			banks[index % len(banks)]++
			blocks--
		}
	}
	// fmt.Printf("% 3v: %v\n", cycle, banks)

	return strconv.Itoa(cycle), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("0\t2\t7\t0"), "5", ""},
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
