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

func process(r io.Reader) (string, string) {
	xs := []int{}
	scanner := bufio.NewScanner(r)

	for scanner.Scan() {
		if x, err := strconv.Atoi(scanner.Text()); err == nil {
			xs = append(xs, x)
		}
	}
	xs2 := make([]int, len(xs))
	copy(xs2, xs)

	i := 0
	ctr := 0
	for i < len(xs) {
		i, xs[i] = i+xs[i], xs[i]+1
		ctr++
	}
	
	i = 0
	ctr2 := 0
	for i < len(xs2) {
		if xs2[i] >= 3 {
			i, xs2[i] = i+xs2[i], xs2[i]-1
		} else {
			i, xs2[i] = i+xs2[i], xs2[i]+1
		}
		ctr2++
	}

	return strconv.Itoa(ctr), strconv.Itoa(ctr2)
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("0\n3\n0\n1\n-3"), "5", ""},
		{strings.NewReader("0\n3\n0\n1\n-3"), "", "10"},
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
