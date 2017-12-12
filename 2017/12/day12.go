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

type node int

func (n node) in(ns []node) bool {
	for i := range ns {
		if ns[i] == n {
			return true
		}
	}
	return false
}

func traverse_bfs(from node, pipes map[node]map[node]bool) []node {
	queue := []node{from}
	visited := []node{from}
	for {
		if len(queue) == 0 {
			break
		}
		x := queue[0]
		queue = queue[1:]
		for y,_ := range pipes[x] {
			if !y.in(visited) {
				visited = append(visited, y)
				queue = append(queue, y)
			}
		}
	}
	return visited
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)
	// adjacency matrix
	pipes := map[node]map[node]bool{}

	for scanner.Scan() {
		//fmt.Printf("pipes: %v\n", pipes)
		line := scanner.Text()
		a := strings.Split(line, " <-> ")
		x,err := strconv.Atoi(a[0])
		if err != nil {
			continue
		}
		new_pipe := []node{node(x)}
		for _, v := range strings.Split(a[1], ", ") {
			x,err = strconv.Atoi(v)
			if err != nil {
				continue
			}
			new_pipe = append(new_pipe, node(x))
		}

		// add elements to pipes
		for i, v := range new_pipe {
			for j := i+1; j<len(new_pipe); j++ {
				w := new_pipe[j]

				if _,ok := pipes[v]; ok {
					pipes[v][w] = true
				} else {
					pipes[v] = map[node]bool{w: true}
				}

				if _,ok := pipes[w]; ok {
					pipes[w][v] = true
				} else {
					pipes[w] = map[node]bool{v: true}
				}
			}
		}
	}
	// fmt.Printf("pipes: %v\n", pipes)
	reachable := traverse_bfs(0, pipes)
	return strconv.Itoa(len(reachable)), ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5\n"), "6", ""},
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
