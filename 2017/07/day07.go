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

type tower_weight struct {
	node_weight int
	disc_load int
}

func (t tower_weight) sum() int {
	return t.node_weight + t.disc_load
}

func get_tower_weight(node string, nodes map[string]*tree) (int, int) {
	sum := 0
	tower_weights := make([]tower_weight, len(nodes[node].children))
	target := -1
	for i, child := range nodes[node].children {

		sub_tower, target_w := get_tower_weight(child, nodes)
		if target_w != -1 {
			// keep track of bad weight of child node
			target = target_w
		}

		tower_weights[i] = tower_weight{
			node_weight: nodes[child].weight,
			disc_load: sub_tower,
		}

		sum += tower_weights[i].sum()
	}

	bad_index := -1
	// find a weight sum that does not match the others in the list
	for i := 2; i < len(tower_weights); i++ {
		if tower_weights[0].sum() != tower_weights[1].sum() {
			if tower_weights[i].sum() == tower_weights[0].sum() {
				bad_index = 1
				break
			} else {
				bad_index = 0
				break
			}
		} else {
			if tower_weights[i].sum() != tower_weights[0].sum() {
				// found bad weight at index i!
				bad_index = i
				break
			}
		}
	}
	if bad_index != -1 {
		if target == -1 {
			// don't overwrite an existing target weight
			if bad_index == 0 {
				// target = weight of other towers minus weight of this child's disc
				target = tower_weights[1].sum() - tower_weights[bad_index].disc_load
			} else {
				target = tower_weights[0].sum() - tower_weights[bad_index].disc_load
			}
			//fmt.Printf("index = %v at node %v, child: %v, child_weight: %v, child_disc_load: %v, target: %v, tower_weights: %v\n", bad_index, node, nodes[node].children[bad_index], tower_weights[bad_index].node_weight, tower_weights[bad_index].disc_load, target, tower_weights[2].sum())
		}
	}

	return sum, target
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

	_, target := get_tower_weight(root, nodes)

	return root, strconv.Itoa(target)
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)\n"), "tknk", "60"},
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
