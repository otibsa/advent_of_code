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

func apply_moves(xs []byte, moves []string) {
	tmp := make([]byte, len(xs))
	for _,move := range moves {
		switch {
		case move[0] == 's':
			spin_size, err := strconv.Atoi(string(move[1:]))
			if err != nil {
				break
			}
			copy(tmp, xs)
			for i:=0; i<len(xs); i++ {
				xs[(i+spin_size)%len(xs)] = tmp[i]
			}

		case move[0] == 'x':
			sA := strings.Split(string(move[1:]), "/")[0]
			sB := strings.Split(string(move[1:]), "/")[1]
			A, err := strconv.Atoi(sA)
			if err != nil {
				break
			}
			B, err := strconv.Atoi(sB)
			if err != nil {
				break
			}
			xs[A], xs[B] = xs[B], xs[A]

		case move[0] == 'p':
			pA := []byte(strings.Split(string(move[1:]), "/")[0])[0]
			pB := []byte(strings.Split(string(move[1:]), "/")[1])[0]
			j := -1
			for i:=0; i<len(xs); i++ {
				if xs[i] == pA || xs[i] == pB {
					if j == -1 {
						j = i
					} else {
						xs[j], xs[i] = xs[i], xs[j]
						break
					}
				}
			}
		}
	}
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)
	scanner.Split(SplitCommas)

	programs := make([]byte, program_count)
	initial_programs := make([]byte, program_count)
	programs_one_round := make([]byte, program_count)

	for i:=0; i<program_count; i++ {
		programs[i] = 'a'+byte(i)
	}
	copy(initial_programs, programs)

	moves := []string{}
	for scanner.Scan() {
		move := strings.TrimSpace(scanner.Text())
		moves = append(moves, move)
	}

	cycle_length := 1
	// fmt.Printf("000: %v\n", string(programs))
	for round:=1; round<100; round++ {
		apply_moves(programs, moves)
		// fmt.Printf("%03v: %v\n", round, string(programs))
		if round == 1 {
			// part 1
			copy(programs_one_round, programs)
		}
		all_same := true
		for i,_ := range programs {
			if programs[i] != initial_programs[i] {
				all_same = false
				break
			}
		}
		if all_same {
			cycle_length = round
			break
		}
	}

	copy(programs, initial_programs)
	for round:=0; round<(1000000000 % cycle_length); round++ {
		apply_moves(programs, moves)
	}

	return string(programs_one_round), string(programs)
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	program_count = 5
	tests := []test_input{
		// {strings.NewReader("s1,x3/4,pe/b"), "baedc", "ceadb"},
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
