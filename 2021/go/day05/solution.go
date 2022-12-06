package day05

import (
	"io"
	"strconv"
)

type Solution struct {
	lines []Line
}

func (s *Solution) Name() string {
	return "Hydrothermal Venture"
}

func (s *Solution) Load(r io.Reader) error {
	lines, err := Parse(r)
	if err != nil {
		return err
	}

	s.lines = lines

	return nil
}

func (s *Solution) PartOne() (string, error) {
	noDiagonals := make([]Line, 0, len(s.lines))

	for _, line := range s.lines {
		if line.LineType() != Diagonal {
			noDiagonals = append(noDiagonals, line)
		}
	}

	return strconv.Itoa(CountHotSpots(noDiagonals)), nil
}

func (s *Solution) PartTwo() (string, error) {
	return strconv.Itoa(CountHotSpots(s.lines)), nil
}
