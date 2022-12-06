package day03

import (
	"fmt"
	"io"
)

type Solution struct {
	lines [][]string
}

func (s *Solution) Name() string {
	return "Binary Diagnostic"
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
	power, err := PowerConsumption(s.lines)
	if err != nil {
		return "", err
	}

	return fmt.Sprintf("%v", power), nil
}
func (s *Solution) PartTwo() (string, error) {
	life, err := LifeSupport(s.lines)
	if err != nil {
		return "", err
	}

	return fmt.Sprintf("%v", life), nil
}
