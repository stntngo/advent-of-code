package day08

import (
	"io"
	"strconv"
)

type Solution struct {
	signals []Signal
}

func (s *Solution) Name() string {
	return "Seven Segment Search"
}

func (s *Solution) Load(r io.Reader) error {
	signals, err := Parse(r)
	if err != nil {
		return err
	}

	s.signals = signals

	return nil
}

func (s *Solution) PartOne() (string, error) {
	return strconv.Itoa(EasyDigitCount(s.signals)), nil
}

func (s *Solution) PartTwo() (string, error) {
	return strconv.Itoa(SignalOutputSum(s.signals)), nil
}
