package main

import (
	"fmt"
	"io"
	"os"
	"strings"
	"bufio"
	"math"
	"strconv"
)

type test_input struct {
	input    io.Reader
	output_1 string
	output_2 string
}

func checksum(line string) (int, int) {
	var low, high int = math.MaxInt64, math.MinInt64
	_,_ = low, high
	xs := []int{}  // part 2
	var sum_1, sum_2 int

	// part 1
	for _, s := range strings.Fields(line) {
		x, err := strconv.Atoi(s)
		if err != nil {
			continue
		}

		low = int(math.Min(float64(low), float64(x)))
		high = int(math.Max(float64(high), float64(x)))

		xs = append(xs, x)
	}
	sum_1 = high-low

	// part 2
	found := false
	i, j := 0, 0
	for i = 0; i < len(xs)-1; i++ {
		
		for j = i+1; j < len(xs); j++ {
			// iterate over the remainder of the list
			if xs[i] % xs[j] == 0 || xs[j] % xs[i] == 0 {
				found = true
				break
			}
		}
		if found {
			break
		}
	}
	if found {
		if xs[i] > xs[j] {
			sum_2 = xs[i] / xs[j]
		} else {
			sum_2 = xs[j] / xs[i]
		}
	}

	return sum_1, sum_2
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)

	var sum_1, sum_2 int
	for scanner.Scan() {
		ls_1, ls_2 := checksum(scanner.Text())

		sum_1 += ls_1
		sum_2 += ls_2
	}
	return strconv.Itoa(int(sum_1)), strconv.Itoa(int(sum_2))
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("5  \t1 9 5\n7 5 3\n2 4 6 8"), "18", ""},
		{strings.NewReader("5 9 2 8\n9 4 7 3\n3 8 6 5"), "", "9"},
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
