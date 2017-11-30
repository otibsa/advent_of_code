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
)

func check_err(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func setup_day(day int, session_cookie string) {

	fmt.Printf("%02v\n", day)
	instruction_url := fmt.Sprintf("http://adventofcode.com/2016/day/%v", day)

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
	if len(os.Args) != 2 {
		fmt.Println("You need to provide value of session cookie from adventofcode.com!\n./setup.go session=...")
		fmt.Println(os.Args)
		os.Exit(1)
	}
	session_cookie := os.Args[1]
	for i := 1; i <= 25; i++ {
		setup_day(i, session_cookie)
		fmt.Println("")
		// be nice to the server
		time.Sleep(10 * time.Second)
	}
}
