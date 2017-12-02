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

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)

	var sum int64
	_ = sum
	for scanner.Scan() {
		var low, high int64 = math.MaxInt64, math.MinInt64
		_,_ = low, high

		for _, s := range strings.Fields(scanner.Text()) {
			x, err := strconv.Atoi(s)
			if err != nil {
				continue
			}

			low = int64(math.Min(float64(low), float64(x)))
			high = int64(math.Max(float64(high), float64(x)))
			fmt.Printf("x=%v    low=%v    high=%v    sum=%v\n", x, low, high, sum)
		}
		fmt.Println()
		sum += high - low
	}
	return strconv.Itoa(int(sum)), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("5  \t1 9 5\n7 5 3\n2 4 6 8"), "18", ""},
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
