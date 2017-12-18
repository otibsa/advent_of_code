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

const frequency_reg = 0
const recovered_frequency_reg = 1

func do(line []string, registers map[byte]int, pc *int, pid int, nr_sent *int, c_send chan<- int, c_recv <-chan int, done chan int) {
	regX := []byte(line[1])[0]
	x, err := strconv.Atoi(line[1])
	if err != nil {
		x = registers[regX]
	}

	var regY byte = 0
	var y int = 0
	if len(line) > 2 {
		regY = []byte(line[2])[0]
		y, err = strconv.Atoi(line[2])
		if err != nil {
			y = registers[regY]
		}
	}

	// fmt.Printf("%02v: %v, regX: %v, x: %v, regY: %v, y: %v, registers: %v\n", *pc, line, regX, x, regY, y, registers)
	*pc++
	switch line[0] {
	case "set":
		registers[regX] = y

	case "add":
		registers[regX] += y

	case "mul":
		registers[regX] *= y

	case "mod":
		registers[regX] = registers[regX] % y

	case "jgz":
		if x > 0 {
			*pc-- //revert previous increment
			*pc += y
		}

	case "snd":
		//fmt.Printf("(snd %v), registers: %v\n", frequency_reg, registers)
		if c_send == nil {
			registers[frequency_reg] = x
		} else {
			c_send <- x
			*nr_sent++
		}

	case "rcv":
		if c_recv == nil {
			if x != 0 {
				registers[recovered_frequency_reg] = registers[frequency_reg]
				// don't want to run forever
				*pc = -1
			}
		} else {
			// hacky: just print nr_sent before we might run into a deadlock
			fmt.Printf("[%v] nr_sent = %v\n", pid, *nr_sent)
			registers[regX] = <-c_recv
		}
	}
}

func p(pid int, instructions [][]string, cs []chan int, done chan int) {
	registers := map[byte]int{'p': pid}
	nr_sent := 0
	pc := 0

	for pc<len(instructions) && pc != -1 {
		do(instructions[pc], registers, &pc, pid, &nr_sent, cs[pid], cs[(pid+1)%2], done)
	}

	if done != nil {
		done <- nr_sent
	}
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)

	registers := map[byte]int{}
	instructions := [][]string{}
	for scanner.Scan() {
		line := strings.Fields(strings.TrimSpace(scanner.Text()))
		instructions = append(instructions, line)
	}

	pc := 0
	for pc<len(instructions) && pc != -1 {
		do(instructions[pc], registers, &pc, 0, nil, nil, nil, nil)
	}

	cs := []chan int{make(chan int, 1000), make(chan int, 1000)}
	done := []chan int{make(chan int), make(chan int)}
	go p(0, instructions, cs, done[0])
	go p(1, instructions, cs, done[1])

	<-done[0]
	part2 := <-done[1]
	return strconv.Itoa(registers[recovered_frequency_reg]), strconv.Itoa(part2)
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		/*{strings.NewReader(`set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2`), "4", ""},
		{strings.NewReader(`snd 1
		snd 2
		snd p
		rcv a
		rcv b
		rcv c
		rcv d`), "", "3"},*/
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
