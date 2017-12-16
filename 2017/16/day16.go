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

var program_count int

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

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)
	scanner.Split(SplitCommas)

	programs := make([]byte, program_count)
	for i:=0; i<program_count; i++ {
		programs[i] = 'a'+byte(i)
	}
	tmp := make([]byte, program_count)

	for scanner.Scan() {
		s := strings.TrimSpace(scanner.Text())
		//fmt.Println("programs: %v\n", string(programs))
		switch {
		case s[0] == 's':
			spin_size, err := strconv.Atoi(string(s[1:]))
			if err != nil {
				break
			}
			copy(tmp, programs)
			for i:=0; i<program_count; i++ {
				programs[(i+spin_size)%len(programs)] = tmp[i]
			}

		case s[0] == 'x':
			sA := strings.Split(string(s[1:]), "/")[0]
			sB := strings.Split(string(s[1:]), "/")[1]
			A, err := strconv.Atoi(sA)
			if err != nil {
				break
			}
			B, err := strconv.Atoi(sB)
			if err != nil {
				break
			}
			programs[A], programs[B] = programs[B], programs[A]

		case s[0] == 'p':
			pA := []byte(strings.Split(string(s[1:]), "/")[0])[0]
			pB := []byte(strings.Split(string(s[1:]), "/")[1])[0]
			j := -1
			for i:=0; i<program_count; i++ {
				if programs[i] == pA || programs[i] == pB {
					if j == -1 {
						j = i
					} else {
						programs[j], programs[i] = programs[i], programs[j]
						break
					}
				}
			}
		}
	}
	//fmt.Println("programs: %v\n", string(programs))
	return string(programs), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	program_count = 5
	tests := []test_input{
		{strings.NewReader("s1,x3/4,pe/b"), "baedc", ""},
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

	program_count = 16
	part1, part2 := process(input)
	fmt.Println("Solution for part 1: \"" + part1 + "\"")
	fmt.Println("Solution for part 2: \"" + part2 + "\"")
}

func check_err(e error) {
	if e != nil {
		panic(e)
	}
}
