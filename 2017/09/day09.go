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

	in_garbage := false
	next_canceled := false
	group_level := 0
	group_counter := 0
	score := 0
	garbage_counter := 0
	for {
		_, err := r.Read(buf)
		if err == io.EOF {
			break
		}
		c := buf[0]
		// fmt.Printf("garb: %v, canc: %v, level: %v, ctr: %v, score: %v, gc: %v, READ: '%c'\n", in_garbage, next_canceled, group_level, group_counter, score, garbage_counter, c)

		if next_canceled {
			next_canceled = false
			continue
		}
		switch c {
		case '{':
			if in_garbage {
				garbage_counter++
				break
			}
			group_counter++
			group_level++
			score += group_level // after increment
		case '}':
			if in_garbage {
				garbage_counter++
				break
			}
			group_level--
		case '<':
			if in_garbage {
				garbage_counter++
				break
			}
			in_garbage = true
		case '>':
			in_garbage = false
		case '!':
			next_canceled = true
		default:
			if in_garbage {
				garbage_counter++
			}
			//fmt.Printf("got '%c'\n", c)
		}
	}
	return strconv.Itoa(score), strconv.Itoa(garbage_counter)
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

tests := []test_input{
	{strings.NewReader("{}"), "1", ""},
	{strings.NewReader("{{{}}}"), "6", ""},
	{strings.NewReader("{{},{}}"), "5", ""},
	{strings.NewReader("{{{},{},{{}}}"), "16", ""},
	{strings.NewReader("{<a>,<a>,<a>,<a>}"), "1", ""},
	{strings.NewReader("{{<ab>},{<ab>},{<ab>},{<ab>}}"), "9", ""},
	{strings.NewReader("{{<!!>},{<!!>},{<!!>},{<!!>}}"), "9", ""},
	{strings.NewReader("{{<a!>},{<a!>},{<a!>},{<ab>}}"), "3", ""},
	{strings.NewReader("<>"), "", "0"},
	{strings.NewReader("<random characters>"), "", "17"},
	{strings.NewReader("<<<<>"), "", "3"},
	{strings.NewReader("<{!>}>"), "", "2"},
	{strings.NewReader("<!!>"), "", "0"},
	{strings.NewReader("<!!!>>"), "", "0"},
	{strings.NewReader(`<{o"i!a,<{i<a>`), "", "10"},
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
