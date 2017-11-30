package main

import (
	"fmt"
	"io/ioutil"
)

func part1(input string) {
	fmt.Println(input)
}

func part2(input string) {
	fmt.Println(input)
}

func main() {
	input, err := ioutil.ReadFile("input.txt")
	check_err(err)

	part1(string(input))
	//part2(string(input))
}

func check_err(e error) {
	if e != nil {
		panic(e)
	}
}
