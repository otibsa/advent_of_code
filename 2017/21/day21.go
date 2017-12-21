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

type field [][]byte

func (f field) cut(x, y, size int) field {
	if len(f) < y+size {
		return nil
	}
	f2 := make(field, size)
	for r:=0; r<size; r++ {
		if len(f[y+r]) < x+size {
			return nil
		}
		f2[r] = f[y+r][x:(x+size)]
	}
	return f2
}

func (f field) str() string {
	s := fmt.Sprintf("%v", string(f[0]))
	for r:=1; r<len(f); r++ {
		s += fmt.Sprintf("/%v", string(f[r]))
	}
	return s
}

func (f *field) replace(x,y int, f2 field) {
	f1 := *f
	if len(f1) < y+len(f2) {
		return
	}
	for r:=0; r<len(f2); r++ {
		if len(f1[y+r]) < x+len(f2[r]) {
			return
		}
		for c:=0; c<len(f2[r]); c++ {
			f1[y+r][x+c] = f2[r][c]
		}
	}
}

func (f field) cp() field {
	tmp := make(field, len(f))
	for r:=0; r<len(f); r++ {
		tmp[r] = make([]byte, len(f[r]))
		for c:=0; c<len(f[r]); c++ {
			tmp[r][c] = f[r][c]
		}
	}

	return tmp
}

func (f field) mh() field {
	tmp := f.cp()
	size := len(f)
	for r:=0; r<size/2; r++ {
		tmp[r] = f[size-r-1][:]
		tmp[size-r-1] = f[r][:]
	}
	return tmp
}

func (f field) mv() field {
	tmp := f.cp()
	size := len(f)
	for r:=0; r<size; r++ {
		for c:=0; c<size/2; c++ {
			tmp[r][c] = f[r][size-c-1]
			tmp[r][size-c-1] = f[r][c]
		}
	}
	return tmp
}

func (f field) rot() field {
	tmp := f.cp()
	size := len(f)
	for r:=0; r<size; r++ {
		for c:=0; c<size; c++ {
			tmp[r][c] = f[size-c-1][r]
		}
	}
	return tmp
}

func (f field) count() int {
	sum := 0
	for r:=0; r<len(f); r++ {
		for c:=0; c<len(f[r]); c++ {
			if f[r][c] == byte('#') {
				sum++
			}
		}
	}
	return sum
}

func construct(s string) field {
	lines := strings.Split(s, "/")
	f := make(field, len(lines))
	for r:=0; r<len(lines); r++ {
		f[r] = []byte(lines[r])
	}

	return f
}

func do_step(f field, rules map[string]string) field {
	f2 := field{}
	replaced := false
	for _,sub_size := range []int{2,3} {
		if len(f) % sub_size == 0 {
			n_subs := len(f) / sub_size
			f2 = make(field, (sub_size+1)*n_subs)
			for r:=0; r<(sub_size+1)*n_subs; r++ {
				f2[r] = make([]byte, (sub_size+1)*n_subs)
			}
			for y:=0; y<n_subs; y++ {
				for x:=0; x<n_subs; x++ {
					sub := f.cut(x*sub_size, y*sub_size, sub_size)
					transforms := []field{sub, sub.mv(), sub.mv().mh(), sub.mh(), sub.rot(), sub.rot().mv(), sub.rot().mv().mh(), sub.rot().mh()}
					for _,t := range transforms {
						v, ok := rules[t.str()]
						if ok {
							replaced = true
							f2.replace(x*(sub_size+1), y*(sub_size+1), construct(v))
							break
						}
					}
					if !replaced {
						return f
					}
				}
			}
		}
		if replaced {
			break
		}
	}
	if replaced {
		return f2
	} else {
		return f
	}
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)
	rules := map[string]string{}
	f := construct(".#./..#/###")
	
	for scanner.Scan() {
		line := strings.Split(strings.TrimSpace(scanner.Text()), " => ")
		if len(line) < 2 {
			continue
		}
		rules[line[0]] = line[1]
	}

	for step:=1; step<=5; step++ {
		f = do_step(f, rules)
	}
	return strconv.Itoa(f.count()), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"), "12", ""},
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
