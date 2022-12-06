package day09

import (
	"io"
	"strconv"
)

type Solution struct {
	hm HeightMap
}

func (s *Solution) Name() string {
	return "Smoke Basin"
}

func (s *Solution) Load(r io.Reader) error {
	hm, err := Parse(r)
	if err != nil {
		return err
	}

	s.hm = hm

	return nil
}

func (s *Solution) PartOne() (string, error) {
	return strconv.Itoa(s.hm.RiskLevel()), nil
}

func (s *Solution) PartTwo() (string, error) {
	return strconv.Itoa(s.hm.BasinFactor()), nil
}
