package main

import (
	"fmt"
	"io"
	"os"
	"strings"
	"strconv"
	"io/ioutil"
)

type test_input struct {
	input    io.Reader
	output_1 string
	output_2 string
}

func process(r io.Reader) (string, string) {
	buf, err := ioutil.ReadAll(r)
	buf = []byte(strings.TrimSpace(string(buf)))
	check_err(err)

	sum_1, sum_2 := 0, 0

	add_flag := false
	for i := range buf {

		x, err := strconv.Atoi(string(buf[i]))
		if err != nil {
			continue
		}

		// part 1
		if buf[i] == buf[(i+1)%len(buf)] {
			if !add_flag {
				// add the skipped digit
				//sum += x
			}
			add_flag = true
			sum_1 += x
		} else {
			add_flag = false
		}

		// part 2
		if (i < len(buf)/2) && (buf[i] == buf[i+len(buf)/2]) {
			sum_2 += 2*x
		}
	}
	return strconv.Itoa(sum_1), strconv.Itoa(sum_2)
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
		{strings.NewReader("1212"), "", "6"},
		{strings.NewReader("1221"), "", "0"},
		{strings.NewReader("123425"), "", "4"},
		{strings.NewReader("123123"), "", "12"},
		{strings.NewReader("12131415"), "", "4"},
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
