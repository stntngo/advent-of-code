package day13

import (
	"bufio"
	"errors"
	"io"
	"regexp"
	"strconv"
	"strings"
)

func Transpose(lines [][]string) [][]string {
	out := make([][]string, 0, len(lines[0]))

	for i := 0; i < len(lines[0]); i++ {
		line := make([]string, 0, len(lines))
		for _, row := range lines {
			line = append(line, row[i])
		}

		out = append(out, line)
	}

	return out
}

func Abs(x int) int {
	if x < 0 {
		return -1 * x
	}

	return x
}

var (
	instruction = regexp.MustCompile("([xy])=(\\d+)")
)

type Point struct {
	x, y int
}

func (p Point) Reflect(axis Direction, value int) Point {
	switch axis {
	case Vertical:
		return Point{
			x: value - Abs(value-p.x),
			y: p.y,
		}
	case Horizontal:
		return Point{
			x: p.x,
			y: value - Abs(value-p.y),
		}
	}

	panic("unreachable")
}

type Grid map[Point]int

func (g Grid) Max() (int, int) {
	var x, y int

	for p := range g {
		if p.x > x {
			x = p.x
		}

		if p.y > y {
			y = p.y
		}
	}

	return x, y
}

func (g Grid) Lines() [][]string {
	mx, my := g.Max()

	lines := make([][]string, my+1)
	for y := 0; y <= my; y++ {
		line := make([]string, mx+1)
		for x := 0; x <= mx; x++ {
			if _, ok := g[Point{x, y}]; ok {
				line[x] = "#"
			} else {
				line[x] = " "
			}

		}

		lines[y] = line
	}

	return lines
}

type Direction int

const (
	Vertical Direction = iota + 1
	Horizontal
)

type Instruction struct {
	Direction Direction
	Value     int
}

func ParseInstruction(raw string) (Instruction, error) {
	matches := instruction.FindAllStringSubmatch(raw, 1)

	if len(matches) != 1 {
		return Instruction{}, errors.New("unexpected match length")
	}

	if len(matches[0]) != 3 {
		return Instruction{}, errors.New("mismatch")
	}

	var inst Instruction
	switch matches[0][1] {
	case "x":
		inst.Direction = Vertical
	case "y":
		inst.Direction = Horizontal
	default:
		return Instruction{}, errors.New("unknown axis")
	}

	val, err := strconv.Atoi(matches[0][2])
	if err != nil {
		return Instruction{}, err
	}

	inst.Value = val

	return inst, nil
}

func ParsePoint(s string) (Point, error) {
	parts := strings.Split(s, ",")
	if len(parts) != 2 {
		return Point{}, errors.New("unexepcted number of point parts")
	}

	x, err := strconv.Atoi(parts[0])
	if err != nil {
		return Point{}, err
	}

	y, err := strconv.Atoi(parts[1])
	if err != nil {
		return Point{}, err
	}

	return Point{x, y}, nil
}

func ParsePoints(r io.Reader) (Grid, []Instruction, error) {
	scanner := bufio.NewScanner(r)

	var instructions []Instruction
	grid := make(map[Point]int)

	parser := func(s string) error {
		point, err := ParsePoint(s)
		if err != nil {
			return err
		}

		grid[point]++

		return nil
	}

	for scanner.Scan() {
		text := scanner.Text()
		if text == "" {
			parser = func(s string) error {
				inst, err := ParseInstruction(s)
				if err != nil {
					return err
				}

				instructions = append(instructions, inst)

				return nil
			}

			continue
		}

		if err := parser(text); err != nil {
			return nil, nil, err
		}
	}

	if err := scanner.Err(); err != nil {
		return nil, nil, err
	}

	return grid, instructions, nil
}

func Fold(grid Grid, instructions []Instruction) Grid {
	for _, instruction := range instructions {
		next := make(map[Point]int)
		for point := range grid {
			next[point.Reflect(instruction.Direction, instruction.Value)]++
		}

		grid = next
	}

	return grid
}

func Translate(grid Grid) (string, error) {
	lines := grid.Lines()

	if len(lines) != 6 {
		return "", errors.New("unable to translate grid")
	}

	transposed := Transpose(grid.Lines())

	var output string
	for len(transposed) > 0 {
		var character [][]string
		character, transposed = transposed[:4], transposed[4:]

		if len(transposed) > 0 {
			transposed = transposed[1:]
		}

		output += read(Transpose(character))
	}

	return output, nil

}
