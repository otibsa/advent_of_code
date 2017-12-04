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

func get_anagrams(word string) []string {
	anagrams := []string{}
	if len(word) == 1 {
		return []string{word}
	}
	for i, c := range word {
		rem := word[:i] + word[i+1:]
		for _, anagram := range get_anagrams(rem) {
			anagrams = append(anagrams, string(c)+anagram)
		}
	}
	return anagrams
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)

	valid_counter := 0
	valid_anagram_counter := 0
	var valid, valid_anagram bool
	_ = valid_anagram
	// for each line:
	for scanner.Scan() {
		valid = true
		valid_anagram = true
		// for each word:
		words := strings.Fields(scanner.Text())
		for i := 0; i < len(words)-1; i++ {
			for j := i+1; j < len(words); j++ {
				if words[i] == words[j] {
					valid = false
				}
				for _, anagram := range get_anagrams(words[j]) {
					if words[i] == anagram {
						valid_anagram = false
					}
				}
			}
		}
		if valid {
			valid_counter++
		}
		if valid_anagram {
			valid_anagram_counter++
		}
	}
	return strconv.Itoa(valid_counter), strconv.Itoa(valid_anagram_counter)
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("aa bb cc dd ee"), "1", ""},
		{strings.NewReader("aa bb cc dd aa"), "0", ""},
		{strings.NewReader("aa bb cc dd aaa"), "1", ""},

		{strings.NewReader("abcde fghij"), "", "1"},
		{strings.NewReader("abcde xyz ecdab"), "", "0"},
		{strings.NewReader("a ab abc abd abf abj"), "", "1"},
		{strings.NewReader("iiii oiii ooii oooi oooo"), "", "1"},
		{strings.NewReader("oiii ioii iioi iiio"), "", "0"},
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
