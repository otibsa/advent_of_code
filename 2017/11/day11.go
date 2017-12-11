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

func SplitCommas(data []byte, atEOF bool) (advance int, token []byte, err error) {
	start := 0
  	for start = 0; start < len(data); start++ {
		if data[start] != ',' {
			break
		}
  	}
	for i := start; i < len(data); i++ {
		if data[i] == ',' {
			return i, data[start:i], nil
		}
	}
	if atEOF && len(data) > start {
		return len(data), data[start:], nil
	}
	return start, nil, nil
}

func same_sign(x,y int) bool {
	return (x<0 && y<0) || (x>=0 && y>=0)
}

func sign(x int) int {
	if x >= 0 {
		return 1
	}
	return -1
}

func min(x, y int) int {
	if x <= y {
		return x
	}
	return y
}

func abs(x int) int {
	return sign(x)*x
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)
	scanner.Split(SplitCommas)

	count_n, count_ne, count_se := 0,0,0
	for scanner.Scan() {
		s := strings.TrimSpace(scanner.Text())
		switch s {
			case "n":
				count_n++
			case "ne":
				count_ne++
			case "se":
				count_se++
			case "s":
				count_n--
			case "sw":
				count_ne--
			case "nw":
				count_se--
			default:
				fmt.Printf("error: got \"%v\"\n", s)
		}
	}
	to_cancel := 0
	if sign(count_n) == sign(count_se) && count_n != 0 && count_se != 0 {
		// fmt.Printf("n+se=ne\n")
		to_cancel = sign(count_n) * min(abs(count_n), abs(count_se))
	} else if sign(count_n) != sign(count_ne) && count_n != 0 && count_ne != 0 {
		// fmt.Printf("n-ne=-e\n")
		to_cancel = sign(count_n) * min(abs(count_n), abs(count_ne))
	} else if sign(count_ne) != sign(count_se) && count_ne != 0 && count_se != 0 {
		// fmt.Printf("ne-se=n\n")
		to_cancel = sign(count_se) * min(abs(count_ne), abs(count_se))
	}
	count_ne += to_cancel
	count_n -= to_cancel
	count_se -= to_cancel

	steps := abs(count_n) + abs(count_ne) + abs(count_se)
	// fmt.Printf("steps=%v\n\n\n", steps)

	return strconv.Itoa(steps), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("ne,ne,ne"), "3", ""},
		{strings.NewReader("ne,ne,sw,sw"), "0", ""},
		{strings.NewReader("ne,ne,s,s"), "2", ""},
		{strings.NewReader("se,sw,se,sw,sw"), "3", ""},
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
