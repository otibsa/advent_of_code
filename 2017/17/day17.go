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

func p(buffer []int, pos int) {
	for i:=0; i<len(buffer); i++ {
		if i == pos {
			fmt.Printf("(%v)", buffer[i])
		} else {
			fmt.Printf(" %v ", buffer[i])
		}
	}
	fmt.Printf("\n")
}

func process(r io.Reader) (string, string) {
	buf := make([]byte, 10)
	n,err := r.Read(buf)
	if err != nil {
		return "",""
	}
	steps, err := strconv.Atoi(strings.TrimSpace(string(buf[:n])))
	if err != nil {
		fmt.Println(err)
		steps = 0
	}

	buffer := []int{0}
	pos := 0

	for new_value:=1; new_value<=2017; new_value++ {
		pos = (pos+steps) % len(buffer)
		buffer = append(buffer[:pos+1], append([]int{new_value}, buffer[pos+1:]...)...)
		pos = (pos+1) % len(buffer)
	}

	return strconv.Itoa(buffer[(pos+1)%len(buffer)]), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("3"), "638", ""},
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
