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
	scanner := bufio.NewScanner(r)
	list := make([]int, 256)
	for i:=0; i<len(list); i++ {
		list[i] = i
	}
	lengths := []int{}
	current_position := 0
	skip_size := 0
	if scanner.Scan() {
		for _, s := range strings.Split(scanner.Text(), ",") {
			if l, err := strconv.Atoi(s); err == nil {
				lengths = append(lengths, l)
			}
		}
	}
	for _, l := range lengths {
		// reverse the next l elements from the current position
		for i:=0; i<l/2; i++ {
			list[(current_position+i)%len(list)], list[(current_position+l-1-i)%len(list)] = list[(current_position+l-1-i)%len(list)], list[(current_position+i)%len(list)]
		}
		current_position = (current_position + l + skip_size) % len(list)
		skip_size++
	}
	//fmt.Printf("list: %v\n", list)

	return strconv.Itoa(list[0]*list[1]), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		//{strings.NewReader("3,4,1,5"), "12", ""},
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
