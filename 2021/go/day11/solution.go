package day11

import (
	"io"
	"strconv"
)

type Solution struct {
	cavern Cavern
}

func (s *Solution) Name() string {
	return "Dumbo Octopus"
}

func (s *Solution) Load(r io.Reader) error {
	cavern, err := ParseCavern(r)
	if err != nil {
		return err
	}

	s.cavern = cavern

	return nil

}

func (s *Solution) PartOne() (string, error) {
	var total int

	cavern := s.cavern.Copy()
	for i := 0; i < 100; i++ {
		total += cavern.Step()
	}

	return strconv.Itoa(total), nil
}

func (s *Solution) PartTwo() (string, error) {
	cavern := s.cavern.Copy()

	var step int
	for {
		step++
		if cavern.Step() == (_SIZE * _SIZE) {
			return strconv.Itoa(step), nil
		}
	}
}
