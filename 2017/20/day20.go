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

type particle struct {
	pX, pY, pZ int
	vX, vY, vZ int
	aX, aY, aZ int
}

type position struct {
	pX, pY, pZ int
}

func abs(x int) int {
	if x >= 0 {
		return x
	} else {
		return -x
	}
}

func min(ps map[int]*particle) (int,int) {
	m_index,m_dist := 0, ps[0].dist()
	for i:=1; i<len(ps); i++ {
		d := ps[i].dist()
		if d < m_dist {
			m_index, m_dist = i, d
		}
	}
	return m_index, m_dist
}

func (p particle) dist() int {
	return abs(p.pX) + abs(p.pY) + abs(p.pZ)
}

func (p particle) collidesWith(p2 particle) bool {
	return p.pX == p2.pX && p.pY == p2.pY && p.pZ == p2.pZ
}

func tick(p *particle) {
	p.vX += p.aX
	p.vY += p.aY
	p.vZ += p.aZ
	p.pX += p.vX
	p.pY += p.vY
	p.pZ += p.vZ
}

func collide(ps map[int]*particle) {
	for i := range ps {
		if ps[i] == nil {
			continue
		}
		i_collides := false
		for j := range ps {
			if ps[j] == nil || i == j {
				continue
			}
			if ps[i].collidesWith(*ps[j]) {
				i_collides = true
				delete(ps, j)
			}
		}
		if i_collides {
			delete(ps, i)
		}
	}
}

func process(r io.Reader) (string, string) {
	scanner := bufio.NewScanner(r)
	lineRE,_ := regexp.Compile("p=<[[:space:]]*(-?[[:digit:]]+),(-?[[:digit:]]+),(-?[[:digit:]]+)>, v=<[[:space:]]*(-?[[:digit:]]+),(-?[[:digit:]]+),(-?[[:digit:]]+)>, a=<[[:space:]]*(-?[[:digit:]]+),(-?[[:digit:]]+),(-?[[:digit:]]+)>")

	particles := map[int]*particle{}
	particles2 := map[int]*particle{}
	count := 0

	for i:=0; scanner.Scan(); i++ {
		line := scanner.Text()
		match := lineRE.FindStringSubmatch(line)
		pX,_ := strconv.Atoi(match[1])
		pY,_ := strconv.Atoi(match[2])
		pZ,_ := strconv.Atoi(match[3])
		vX,_ := strconv.Atoi(match[4])
		vY,_ := strconv.Atoi(match[5])
		vZ,_ := strconv.Atoi(match[6])
		aX,_ := strconv.Atoi(match[7])
		aY,_ := strconv.Atoi(match[8])
		aZ,_ := strconv.Atoi(match[9])
		particles[i] = &particle{pX:pX, pY:pY, pZ:pZ, vX:vX, vY:vY, vZ:vZ, aX:aX, aY:aY, aZ:aZ}
		particles2[i] = &particle{pX:pX, pY:pY, pZ:pZ, vX:vX, vY:vY, vZ:vZ, aX:aX, aY:aY, aZ:aZ}
	}

	min_index, min_dist := min(particles)
	for step:=1; step<1000; step++ {
		tick(particles[0])
		min_index, min_dist = 0, particles[0].dist()
		for i:=1; i<len(particles); i++ {
			tick(particles[i])
			d := particles[i].dist()
			if d < min_dist {
				min_index, min_dist = i, d
			}
		}

		count = 0
		for i := range particles2 {
			count++
			tick(particles2[i])
		}
		//fmt.Printf("[% 4v] count = %v\n", step, count)
		collide(particles2)

	}

	count = 0
	for _ = range particles2 {
		count++
	}

	return strconv.Itoa(min_index), strconv.Itoa(count)
}

func main() {
	input, err := os.Open("input.txt")
	check_err(err)
	defer input.Close()

	tests := []test_input{
		//{strings.NewReader("p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>\np=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>\n"), "0", ""},
		{strings.NewReader("p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>\np=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>\np=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>\np=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>\n"), "", "1"},
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
