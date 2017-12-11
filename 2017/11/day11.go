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

func distance(pos_n, pos_ne, pos_se int) int {
	to_cancel := 0
	if sign(pos_n) == sign(pos_se) && pos_n != 0 && pos_se != 0 {
		// fmt.Printf("n+se=ne\n")
		to_cancel = sign(pos_n) * min(abs(pos_n), abs(pos_se))
	} else if sign(pos_n) != sign(pos_ne) && pos_n != 0 && pos_ne != 0 {
		// fmt.Printf("n-ne=-e\n")
		to_cancel = sign(pos_n) * min(abs(pos_n), abs(pos_ne))
	} else if sign(pos_ne) != sign(pos_se) && pos_ne != 0 && pos_se != 0 {
		// fmt.Printf("ne-se=n\n")
		to_cancel = sign(pos_se) * min(abs(pos_ne), abs(pos_se))
	}
	pos_ne += to_cancel
	pos_n -= to_cancel
	pos_se -= to_cancel

	return  abs(pos_n) + abs(pos_ne) + abs(pos_se)
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)
	scanner.Split(SplitCommas)

	pos_n, pos_ne, pos_se := 0,0,0
	max_distance := 0
	for scanner.Scan() {
		s := strings.TrimSpace(scanner.Text())
		switch s {
			case "n":
				pos_n++
			case "ne":
				pos_ne++
			case "se":
				pos_se++
			case "s":
				pos_n--
			case "sw":
				pos_ne--
			case "nw":
				pos_se--
			default:
				fmt.Printf("error: got \"%v\"\n", s)
		}
		m_dist := distance(pos_n, pos_ne, pos_se)
		if m_dist > max_distance {
			max_distance = m_dist
		}
	}

	dist := distance(pos_n, pos_ne, pos_se)

	return strconv.Itoa(dist), strconv.Itoa(max_distance)
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
