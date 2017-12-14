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

func count_ones(hex_string string) int {
	sum := 0
	for _,c := range hex_string {
		switch c {
		case '1':
			fallthrough
		case '2':
			fallthrough
		case '4':
			fallthrough
		case '8':
			sum += 1
		case '3':
			fallthrough
		case '5':
			fallthrough
		case '6':
			fallthrough
		case '9':
			fallthrough
		case 'a':
			fallthrough
		case 'c':
			sum += 2
		case '7':
			fallthrough
		case 'b':
			fallthrough
		case 'd':
			fallthrough
		case 'e':
			sum += 3
		case 'f':
			sum += 4
		}
	}
	return sum
}

func knot_hash(input string) string {
	lengths := []byte(input)
	lengths = append(lengths, []byte{17,31,73,47,23}...)
	list := make([]int, 256)
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
	buf := make([]byte, 100)
	n, err := r.Read(buf)
	check_err(err)

	// string(buf) copies the null-bytes !
	// (strings are not null-terminated in Go)
	s := string(buf[:n])
	s = strings.TrimSpace(s)
	square_count := 0

	for i:=0; i<128; i++ {
		t := fmt.Sprintf("%v-%v", s, i)
		kh := knot_hash(t)
		// fmt.Printf("knot_hash(\"%v\") = %v, ones: %v\n", t, kh, count_ones(kh))
		square_count += count_ones(kh)
	}
	return strconv.Itoa(square_count), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("flqrgnkx"), "8108", ""},
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
