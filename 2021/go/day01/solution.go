package day01

import (
	"io"
	"strconv"
)

type Solution struct {
	reading SonarReading
}

func (s *Solution) Name() string {
	return "Sonar Sweep"
}

func (s *Solution) Load(r io.Reader) error {
	reading, err := ParseSonarReading(r)
	if err != nil {
		return err
	}

	s.reading = reading

	return nil
}

func (s *Solution) PartOne() (string, error) {
	return strconv.Itoa(s.reading.DepthIncrease()), nil
}

func (s *Solution) PartTwo() (string, error) {
	return strconv.Itoa(s.reading.SlidingWindow(3).DepthIncrease()), nil
}
