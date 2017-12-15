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

func _gen(seed int, multiplier int, gen chan<- int, stop <-chan bool) {
	x := seed
	done := false
	for {
		// check if we should stop
		select {
		case <-stop:
			done = true
		default:
			done = false
		}
		if done {
			break
		}

		x = (x*multiplier) % 0x7fffffff
		// send x on generator channel:
		gen <- x
	}
	close(gen)
}

func generator(seed int, multiplier int) (<-chan int, chan<- bool) {
	gen := make(chan int)
	stop := make(chan bool, 1)
	go _gen(seed, multiplier, gen, stop)

	return gen, stop
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)

	seedA, seedB := 0,0
	if scanner.Scan() {
		line := strings.Fields(scanner.Text())
		seedA, _ = strconv.Atoi(line[len(line)-1])
	}
	if scanner.Scan() {
		line := strings.Fields(scanner.Text())
		seedB, _ = strconv.Atoi(line[len(line)-1])
	}
	cA, stopA := generator(seedA, 16807)
	cB, stopB := generator(seedB, 48271)

	count := 0
	for i:=0; i<40000000; i++ {
		xA, xB := <-cA, <-cB
		if (xA & 0xffff) == (xB & 0xffff) {
			count++
		}
	}
	stopA <- true
	close(stopA)
	stopB <- true
	close(stopB)

	return strconv.Itoa(count), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		//{strings.NewReader("A uses 65\nB uses 8921"), "588", ""},
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
