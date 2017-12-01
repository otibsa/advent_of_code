package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"regexp"
	"time"
	"strconv"
)

func check_err(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func setup_day(day int, session_cookie string) {

	fmt.Printf("%02v\n", day)
	instruction_url := fmt.Sprintf("http://adventofcode.com/2017/day/%v", day)

	fmt.Println("Get title from instruction page")
	response, err := http.Get(instruction_url)
	check_err(err)

	body, err := ioutil.ReadAll(response.Body)
	response.Body.Close()
	check_err(err)

	r, _ := regexp.Compile("--- (Day .+?) ---")
	if !r.Match(body) {
		log.Fatal("title not matched")
	}
	title := string(r.FindSubmatch(body)[1])

	folder := fmt.Sprintf("%02v", day)
	fmt.Println("Create folder", folder)
	err = os.MkdirAll(folder, 0755)
	check_err(err)

	fmt.Println("Create README file")
	readme_file, err := os.Create(folder + "/README.md")
	check_err(err)
	defer readme_file.Close()

	README_content := `# %v
[Link](%v)

## Part 1
_TODO_

## Part 2
_TODO_
`
	fmt.Fprintf(readme_file, README_content, title, instruction_url)

	fmt.Println("Create template .go file")
	go_file, err := os.Create(fmt.Sprintf("%v/day%02v.go", folder, day))
	check_err(err)
	defer go_file.Close()

	go_content := `package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

type test_input struct {
	input    io.Reader
	output_1 string
	output_2 string
}

func process(r io.Reader) (string, string) {
	buf := make([]byte, 1)

	for {
		_, err := r.Read(buf)
		if err == io.EOF {
			break
		}

		// do stuff with instruction(s) in buf
	}
	return "", ""
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		{strings.NewReader("test input 1"), "", ""},
		{strings.NewReader("test input 2"), "", ""},
		{strings.NewReader("test input 3"), "", ""},
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
`
	fmt.Fprint(go_file, go_content)


	time.Sleep(10 * time.Second)
	fmt.Println("Get input.txt")
	client := &http.Client{}
	req, err := http.NewRequest("GET", instruction_url+"/input", nil)
	check_err(err)


	req.Header.Add("Cookie", session_cookie)
	//response, err = http.G
	response, err = client.Do(req)
	check_err(err)

	puzzle_input, err := os.Create(folder+"/input.txt")
	check_err(err)
	defer puzzle_input.Close()

	_, err = io.Copy(puzzle_input, response.Body)
	check_err(err)
}

func main() {
	if len(os.Args) != 3 {
		fmt.Println("You need to provide the day and the value of session cookie from adventofcode.com!\n./setup.go 9 session=...")
		fmt.Println(os.Args)
		os.Exit(1)
	}
	day, err := strconv.Atoi(os.Args[1])
	check_err(err)

	session_cookie := os.Args[2]
	_ = session_cookie

	setup_day(day, session_cookie)
}
