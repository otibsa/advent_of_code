package main

import (
	"fmt"
	"io"
	"os"
	"strings"
	"bufio"
	"strconv"
)

var list_length int

type test_input struct {
	input    io.Reader
	output_1 string
	output_2 string
}

func knot_hash(input string) string {
	lengths := []byte(input)
	lengths = append(lengths, []byte{17,31,73,47,23}...)
	list := make([]int, list_length)
	for i:=0; i<len(list); i++ {
		list[i] = i
	}
	current_position := 0
	skip_size := 0

	for round:=0; round<64; round++ {
		for _, l := range lengths {
			// reverse the next l elements from the current position
			for i:=0; i<int(l)/2; i++ {
				list[(current_position+i)%len(list)], list[(current_position+int(l)-1-i)%len(list)] = list[(current_position+int(l)-1-i)%len(list)], list[(current_position+i)%len(list)]
			}
			current_position = (current_position + int(l) + skip_size) % len(list)
			skip_size++
		}
	}
	//fmt.Printf("sparse hash: %v\n", list)

	dense_hash := make([]int, 16)
	for i,_ := range dense_hash {
		dense_hash[i] = list[16*i]
		for j:=1; j<16; j++ {
			dense_hash[i] ^= list[16*i+j]
		}
	}
	hex_hash := ""
	for _,h := range dense_hash {
		hex_hash += fmt.Sprintf("%02x", h)
	}
	//fmt.Printf("dense hash: %v\n", hex_hash)
	return hex_hash
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)
	list := make([]int, list_length)
	for i:=0; i<len(list); i++ {
		list[i] = i
	}
	lengths := []int{}
	current_position := 0
	skip_size := 0
	line := ""
	if scanner.Scan() {
		line = scanner.Text()
		for _, s := range strings.Split(line, ",") {
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
	part1 := strconv.Itoa(list[0]*list[1])

	// part2
	part2 := knot_hash(line)

	return part1, part2
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	list_length = 256
	tests := []test_input{
		//{strings.NewReader("3,4,1,5"), "12", ""},
		{strings.NewReader(""), "", "a2582a3a0e66e6e86e3812dcb672a272"},
		{strings.NewReader("AoC 2017"), "", "33efeb34ea91902bb2f59c9920caa6cd"},
		{strings.NewReader("1,2,3"), "", "3efbe78a8d82f29979031a4aa0b16a9d"},
		{strings.NewReader("1,2,4"), "", "63960835bcdc130f0b66d7ff4f6a5a8e"},
	}
	for i, t := range tests {
		if i == 0 {
			//list_length = 5
		} else {
			list_length = 256
		}
		sol_1, sol_2 := process(t.input)
		if t.output_1 != "" && t.output_1 != sol_1 {
			fmt.Printf("Part 1 test input \"%v\" failed: got \"%v\", wanted \"%v\"\n", t.input, sol_1, t.output_1)
		}
		if t.output_2 != "" && t.output_2 != sol_2 {
			fmt.Printf("Part 2 test input \"%v\" failed: got \"%v\", wanted \"%v\"\n", t.input, sol_2, t.output_2)
		}
	}

	list_length = 256
	part1, part2 := process(input)
	fmt.Println("Solution for part 1: \"" + part1 + "\"")
	fmt.Println("Solution for part 2: \"" + part2 + "\"")
}

func check_err(e error) {
	if e != nil {
		panic(e)
	}
}
