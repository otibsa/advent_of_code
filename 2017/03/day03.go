package main

import (
	"fmt"
	"io"
	"os"
	"strings"
	"io/ioutil"
	"strconv"
	"math"
)

type test_input struct {
	input    io.Reader
	output_1 string
	output_2 string
}

type position struct {
	x,y int
}

func hamming_distance(x, y int) int {
	return int(math.Abs(float64(x)) + math.Abs(float64(y)))
}

func get_layer(n int) int {
	// found through trial and error
	return int(math.Floor(0.5*(math.Sqrt(float64(n)-1)+1)))
}

func walk_spiral(n int) int {
	var x, y, layer, layer_low int
	var tp1, tp2, tp3, tp4 int
	
	if n == 1 {
		return 0
	}
	for i := 2; i <= n; i++ {
		
		if get_layer(i) > layer {
			layer = get_layer(i)
			layer_low = int(math.Pow(2*float64(layer)-1, 2) + 1)

			// turning points
			tp1 = layer_low + 1
			tp2 = tp1 + 2 * layer - 1
			tp3 = tp2 + 2 * layer
			tp4 = tp3 + 2 * layer

			//fmt.Printf("[%v]\tlayer_low=%v   tps: %v, %v, %v, %v\n", layer, layer_low, tp1, tp2, tp3, tp4)
		}

		switch {
		case tp1 <= i && i < tp2:
			// heading "north"
			//fmt.Printf("heading north, ")
			y++
		case tp2 <= i && i < tp3:
			// heading "west"
			//fmt.Printf("heading west, ")
			x--
		case tp3 <= i && i < tp4:
			// heading "south"
			//fmt.Printf("heading south, ")
			y--
		case tp4 <= i || i == layer_low:
			// heading "east"
			//fmt.Printf("heading east, ")
			x++
		default:
			fmt.Printf("??? i=%v tp4=%v layer_low=%v:  last case: %v\n", i, tp4, layer_low, (tp4 <= i || i == layer_low))
		}
		//fmt.Printf("i=% 3v   x=% 3v   y=% 3v\n", i, x, y)
	}

	return hamming_distance(x, y)
}

func build_field(n int) []position {
	field := make([]position, n)
	_ = field
	filled_layers := 0
	_ = filled_layers
	for i := 1; i <= n; i++ {
		layer := get_layer(i)
		layer_low := layer*layer+1
		_ = layer_low
	}
	return []position{}
}

func process(r io.Reader) (string, string) {
	buf, err := ioutil.ReadAll(r)
	check_err(err)

	s := strings.TrimSpace(string(buf))
	n, err := strconv.Atoi(s)
	check_err(err)

	return strconv.Itoa(walk_spiral(n)), ""
	// var field []position
	// field = build_field(n)
	// pos := field[n]

	// return strconv.Itoa(hamming_distance(pos.x,pos.y)), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("1"), "0", ""},
		{strings.NewReader("12"), "3", ""},
		{strings.NewReader("23"), "2", ""},
		{strings.NewReader("1024"), "31", ""},
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
