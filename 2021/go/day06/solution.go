package day06

import (
	"fmt"
	"io"
	"io/ioutil"
)

type Solution struct {
	fish LanternFish
}

func (s *Solution) Name() string {
	return "Lanternfish"
}

func (s *Solution) Load(r io.Reader) error {
	b, err := ioutil.ReadAll(r)
	if err != nil {
		return err
	}

	fish, err := Parse(string(b))
	if err != nil {
		return err
	}

	s.fish = fish

	return nil
}

func (s *Solution) PartOne() (string, error) {
	return fmt.Sprintf("%v", SimulatePopulation(s.fish, 80).Pop()), nil
}

func (s *Solution) PartTwo() (string, error) {
	return fmt.Sprintf("%v", SimulatePopulation(s.fish, 256).Pop()), nil
}
