package day14

import (
	"fmt"
	"io"
)

type Solution struct {
	template string
	rules    []Insertion
}

func (s *Solution) Name() string {
	return "Extended Polymerization"
}

func (s *Solution) Load(r io.Reader) error {
	template, rules, err := ParsePolymers(r)
	s.template = template
	s.rules = rules

	return err
}

func (s *Solution) PartOne() (string, error) {
	extended := ExtendPolymer(s.template, s.rules, 10)

	return fmt.Sprintf("%v", PolymerScore(s.template, extended)), nil
}

func (s *Solution) PartTwo() (string, error) {
	extended := ExtendPolymer(s.template, s.rules, 40)

	return fmt.Sprintf("%v", PolymerScore(s.template, extended)), nil
}
