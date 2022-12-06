package day02

import (
	"io"
	"strconv"
)

type Solution struct {
	commands []Command
}

func (s *Solution) Name() string {
	return "Dive!"
}

func (s *Solution) Load(r io.Reader) error {
	commands, err := ParseCommands(r)
	if err != nil {
		return err
	}

	s.commands = commands

	return nil
}

func (s *Solution) PartOne() (string, error) {
	return strconv.Itoa(Vector(s.commands)), nil
}

func (s *Solution) PartTwo() (string, error) {
	return strconv.Itoa(AimVector(s.commands)), nil
}
