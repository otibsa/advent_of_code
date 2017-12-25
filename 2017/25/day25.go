package main

import (
	"fmt"
	"io"
	"os"
	"strings"
	"bufio"
	"regexp"
	"strconv"
)

type test_input struct {
	input    io.Reader
	output_1 string
	output_2 string
}

type tape struct {
	memory map[int]int
	cursor int
	low int
	high int
}

type state_input struct {
	state byte
	value int
}

type state_output struct {
	state byte
	value int
	move int
}

type turing_machine struct {
	state_transitions map[state_input]state_output
	state byte
	t tape
}

func (tm *turing_machine) step() {
	value := tm.t.read()
	//fmt.Printf("[input] state: %c, value: %v\n", tm.state, value)
	out := tm.state_transitions[state_input{state:tm.state, value:value}]
	tm.state = out.state
	tm.t.write(out.value)
	if out.move == 1 {
		tm.t.right()
	} else {
		tm.t.left()
	}
	//fmt.Printf("[output] state: %c, value: %v, move: %v\n", tm.state, value, out.move)
}

func (t tape) read() int {
	return t.memory[t.cursor]
}

func (t *tape) write(x int) {
	t.memory[t.cursor] = x
}

func (t *tape) left() {
	t.cursor--
	if t.cursor < t.low {
		t.low = t.cursor
	}
}

func (t *tape) right() {
	t.cursor++
	if t.cursor > t.high {
		t.high = t.cursor
	}
}

func (t tape) str() string {
	s := "..."
	for i:=t.low; i<=t.high; i++ {
		if i == t.cursor {
			s += fmt.Sprintf("[%v]", t.memory[i])
		} else {
			s += fmt.Sprintf(" %v ", t.memory[i])
		}
	}
	s += "..."
	return s
}

func (t tape) checksum() int {
	sum := 0
	for i:=t.low; i<=t.high; i++ {
		sum += t.memory[i]
	}
	return sum
}

func process(r io.Reader) (string, string) {
	// TODO: build turing machine from input
	scanner := bufio.NewScanner(r)
	var start_state byte
	var steps int
	var state byte
	var value int
	var new_value int
	var move int
	var new_state byte
	r1,_ := regexp.Compile("In state (.):")
	r2,_ := regexp.Compile("If the current value is (.):")
	r3,_ := regexp.Compile("- Write the value (.).")
	r4,_ := regexp.Compile("- Move one slot to the (right|left).")
	r5,_ := regexp.Compile("- Continue with state (.).")
	r6,_ := regexp.Compile("Begin in state (.).")
	r7,_ := regexp.Compile("Perform a diagnostic checksum after ([[:digit:]]+) steps.")
	
	state_transitions := map[state_input]state_output{}

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if match := r6.FindStringSubmatch(line); len(match) > 0 {
			start_state = []byte(match[1])[0]
		}
		if match := r7.FindStringSubmatch(line); len(match) > 0 {
			steps, _ = strconv.Atoi(match[1])
		}
		if match := r1.FindStringSubmatch(line); len(match) > 0 {
			state = []byte(match[1])[0]
		}
		if match := r2.FindStringSubmatch(line); len(match) > 0 {
			value, _ = strconv.Atoi(match[1]) //int([]byte(match[1])[0] - byte('0'))
		}
		if match := r3.FindStringSubmatch(line); len(match) > 0 {
			new_value, _ = strconv.Atoi(match[1]) //int([]byte(match[1])[0] - byte('0'))
		}
		if match := r4.FindStringSubmatch(line); len(match) > 0 {
			if match[1] == "right" {
				move = 1
			} else {
				move = -1
			}
		}
		if match := r5.FindStringSubmatch(line); len(match) > 0 {
			new_state = []byte(match[1])[0]
			state_transitions[state_input{state:state, value:value}] = state_output{state:new_state, value:new_value, move:move}
		}
	}

	t := tape{memory: make(map[int]int)}
	tm := turing_machine{state: start_state, t: t, state_transitions: state_transitions}

	//fmt.Println(tm.t.str())
	for ; steps > 0; steps-- {
		tm.step()
		//fmt.Println(tm.t.str())
	}

	sum := tm.t.checksum()
	return strconv.Itoa(sum), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader(`Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
`), "3", ""},
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
