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

type position struct {
	x,y,heading,steps int
}

func pr(field [][]byte) {
	for y:=0; y<len(field); y++ {
		fmt.Println(string(field[y]))
	}
}

func isLetter(l byte) bool {
	return (l >= 'A' && l <= 'Z') || (l >= 'a' && l <= 'z')
}

func (p *position) next(field [][]byte) byte {
	switch field[p.y][p.x] {
	case ' ':
		return 0
	case '+':
		if p.heading == 0 || p.heading == 2 {
			if field[p.y][p.x-1] == '-' || isLetter(field[p.y][p.x-1]) {
				// west
				p.x = p.x-1
				p.heading = 3
			} else if field[p.y][p.x+1] == '-' || isLetter(field[p.y][p.x+1]) {
				// east
				p.x = p.x+1
				p.heading = 1
			} else {
				return 0
			}
		} else {
			if field[p.y-1][p.x] == '|' || isLetter(field[p.y-1][p.x]) {
				// north
				p.y = p.y-1
				p.heading = 0
			} else if field[p.y+1][p.x] == '|' || isLetter(field[p.y+1][p.x]) {
				// south
				p.y = p.y+1
				p.heading = 2
			} else {
				return 0
			}
		}
	default:
		switch p.heading {
		case 0:
			p.y = p.y-1
		case 1:
			p.x = p.x+1
		case 2:
			p.y = p.y+1
		case 3:
			p.x = p.x-1
		}
	}
	p.steps++
	return field[p.y][p.x]
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)
	field := [][]byte{}

	for scanner.Scan() {
		line := []byte(scanner.Text())
		field = append(field, line)
	}

	heading := 2  // south
	x,y := 0, 0
	for start_x:=0; start_x<len(field[0]); start_x++ {
		if field[0][start_x] == '|' {
			x = start_x
		}
	}
	p := position{x: x, y: y, heading: heading}
	letters := []byte{}

	for b:=byte('|'); b!=0; b=p.next(field){
		if isLetter(b) {
			letters = append(letters, b)
		}
	}
	// fmt.Println(string(letters))
	return string(letters), strconv.Itoa(p.steps) 
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader(`     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 
                `), "ABCDEF", "38"},
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
