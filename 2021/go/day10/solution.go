package day10

import (
	"io"
	"strconv"
)

type Solution struct {
	lines []string
}

func (s *Solution) Name() string {
	return "Syntax Scoring"
}

func (s *Solution) Load(r io.Reader) error {
	lines, err := Lines(r)
	if err != nil {
		return err
	}

	s.lines = lines

	return nil
}

func (s *Solution) PartOne() (string, error) {
	corruption, _ := Score(s.lines)
	return strconv.Itoa(corruption), nil
}

func (s *Solution) PartTwo() (string, error) {
	_, autocomplete := Score(s.lines)
	return strconv.Itoa(autocomplete), nil
}
