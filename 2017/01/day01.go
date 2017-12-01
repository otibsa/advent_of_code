package main

import (
	"fmt"
	"io"
	"os"
	"strings"
	"strconv"
)

type test_input struct {
	input    io.Reader
	output_1 string
	output_2 string
}

func process(r io.Reader) (string, string) {
	buf := make([]byte, 1)

	sum := 0

	var first_digit, previous_digit byte = '#', '#'
	add_flag := false
	for {
		_, err := r.Read(buf)
		if err == io.EOF {
			break
		}

		if first_digit == '#' {
			first_digit = buf[0]
		}

		x, err := strconv.Atoi(string(buf))
		if err != nil {
			continue
		}

		if buf[0] == previous_digit {
			if !add_flag {
				// add the skipped digit
				//sum += x
			}
			add_flag = true
			sum += x
		} else {
			add_flag = false
		}
		fmt.Printf("sum=%v, previous_digit=%c, buf[0]=%c (x=%v), add_flag=%v, first_digit=%c\n", sum, previous_digit, buf[0], x, add_flag, first_digit)
		previous_digit = buf[0]

	}
	if previous_digit == first_digit {
		x, _ := strconv.Atoi(string(first_digit))
		sum += x
	}
	return strconv.Itoa(sum), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("1122"), "3", ""},
		{strings.NewReader("1111"), "4", ""},
		{strings.NewReader("1234"), "0", ""},
		{strings.NewReader("91212129"), "9", ""},
	}
	for _, t := range tests {
		sol_1, sol_2 := process(t.input)
		if t.output_1 != "" && t.output_1 != sol_1 {
			fmt.Printf("Part 1 test input \"%v\" failed: got \"%v\", wanted \"%v\"\n", t.input, sol_1, t.output_1)
		}
		if t.output_2 != "" && t.output_2 != sol_2 {
			fmt.Printf("Part 2 test input \"%v\" failed: got \"%v\", wanted \"%v\"\n", t.input, sol_2, t.output_2)
		}
		fmt.Println()
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
