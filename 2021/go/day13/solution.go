package day13

import (
	"io"
	"strconv"
)

type Solution struct {
	grid         Grid
	instructions []Instruction
}

func (s *Solution) Name() string {
	return "Transparent Origami"
}

func (s *Solution) Load(r io.Reader) error {
	grid, instructions, err := ParsePoints(r)
	if err != nil {
		return err
	}

	s.grid = grid
	s.instructions = instructions

	return nil
}

func (s *Solution) PartOne() (string, error) {
	grid := Fold(s.grid, s.instructions[:1])
	return strconv.Itoa(len(grid)), nil
}

func (s *Solution) PartTwo() (string, error) {
	grid := Fold(s.grid, s.instructions)

	return Translate(grid)
}
