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

type tree struct {
	weight int
	children []string
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)

	nodes := map[string]*tree{}
	roots := map[string]bool{}
	has_parent := map[string]bool{}
	for scanner.Scan() {
		line := strings.Fields(strings.Replace(scanner.Text(), ",", "", -1))
		if len(line) < 2 {
			continue
		}
		name := line[0]
		weight, err := strconv.Atoi(strings.Trim(line[1], "()"))
		if err != nil {
			continue
		}
		children := []string{}
		if len(line) > 3 {
			children = line[3:]
			for _, child := range children {
				has_parent[child] = true
				delete(roots, child)
			}
		}

		node := tree{weight: weight, children: children}
		// don't care if we already saw this node, overwrite the pointer anyway
		nodes[name] = &node

		if !has_parent[name] {
			roots[name] = true
		}
	}
	root := ""
	for k := range roots {
		root = k
		break
	}
	return root, ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)\n"), "tknk", ""},
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
