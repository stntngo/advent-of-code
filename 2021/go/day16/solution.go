package day16

import (
	"io"
	"strconv"
)

type Solution struct {
	root Packet
}

func (s *Solution) Name() string {
	return "Packet Decoder"
}

func (s *Solution) Load(r io.Reader) error {
	packet, err := Parse(r)
	if err != nil {
		return err
	}

	s.root = packet

	return nil
}

func (s *Solution) PartOne() (string, error) {
	return strconv.Itoa(int(s.root.Version())), nil
}

func (s *Solution) PartTwo() (string, error) {
	value, err := Exec(s.root)
	if err != nil {
		return "", err
	}

	return strconv.Itoa(int(value)), nil
}
