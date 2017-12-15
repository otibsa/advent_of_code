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

type node struct {
	x,y int
}

type field [128][128]int

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

func hash2row(hash string) [128]int {
	row := [128]int{}
	for i:=0; i<32; i++ {
		c := hash[i]
		t := ""
		switch c {
		case '0':
			t = "...."
		case '1':
			t = "...#"
		case '2':
			t = "..#."
		case '3':
			t = "..##"
		case '4':
			t = ".#.."
		case '5':
			t = ".#.#"
		case '6':
			t = ".##."
		case '7':
			t = ".###"
		case '8':
			t = "#..."
		case '9':
			t = "#..#"
		case 'a':
			t = "#.#."
		case 'b':
			t = "#.##"
		case 'c':
			t = "##.."
		case 'd':
			t = "##.#"
		case 'e':
			t = "###."
		case 'f':
			t = "####"
		}
		row[4*i+0] = int(t[0])
		row[4*i+1] = int(t[1])
		row[4*i+2] = int(t[2])
		row[4*i+3] = int(t[3])
	}
	return row
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

func (f field) show(compact bool) {
	for i:=0; i<128; i++ {
		for j:=0; j<128; j++ {
			if compact {
				fmt.Printf("%c", byte(f[i][j]))
			} else {
				if byte(f[i][j]) == '#' {
					fmt.Printf("###")
				} else if byte(f[i][j]) == '.' {
					fmt.Printf("...")
				} else {
					fmt.Printf("% 3x", f[i][j])
				}
			}
		}
		fmt.Printf("\n")
	}
}

func (n node) isInside() bool {
	return n.x >= 0 && n.x < 128 && n.y >= 0 && n.y < 128
}

func (f field) get(n node) int {
	return f[n.x][n.y]
}

func (f *field) set(n node, value int) {
	if n.isInside() {
		f[n.x][n.y] = value
	}
}

func (n node) in(ns []node) bool {
	for i := range ns {
		if ns[i] == n {
			return true
		}
	}
	return false
}

func (f *field) flood(from node, value int) {
	queue := []node{from}
	visited := []node{from}
	for {
		if len(queue) == 0 {
			break
		}
		n := queue[0]
		if f.get(n) == '#' {
			f.set(n, value)
		}
		queue = queue[1:]
		neighbors := []node{
			node{x:n.x, y:n.y-1},
			node{x:n.x+1, y:n.y},
			node{x:n.x, y:n.y+1},
			node{x:n.x-1, y:n.y},
		}
		for _,nn := range neighbors {
			if nn.isInside() && f.get(nn) == '#' {
				if !nn.in(visited) {
					visited = append(visited, nn)
					queue = append(queue, nn)
				}
			}
		}
	}
}

func (f field) count_regions() int {
	regions := 0
	for i:=0; i<128; i++ {
		for j:=0; j<128; j++ {
			// start at i,j
			n := node{x:i, y:j}
			if f.get(n) != '#' {
				continue
			}
			regions++
			f.flood(n, regions)
		}
	}
	//f.show(false)
	return regions
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

	var f field

	for i:=0; i<128; i++ {
		t := fmt.Sprintf("%v-%v", s, i)
		kh := knot_hash(t)
		// fmt.Printf("knot_hash(\"%v\") = %v, ones: %v\n", t, kh, count_ones(kh))
		square_count += count_ones(kh)
		f[i] = hash2row(kh)
		//fmt.Println(string(f[i][:]))
	}
	region_count := f.count_regions()
	return strconv.Itoa(square_count), strconv.Itoa(region_count)
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("flqrgnkx"), "8108", "1242"},
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
