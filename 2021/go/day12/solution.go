package day12

import (
	"bytes"
	"io"
	"strconv"
)

type Solution struct {
	p1, p2 CaveSystem
	buffer []byte
}

func (s *Solution) Name() string {
	return "Passage Pathing"
}

func (s *Solution) Load(r io.Reader) error {
	var b1, b2 bytes.Buffer

	w := io.MultiWriter(&b1, &b2)
	if _, err := io.Copy(w, r); err != nil {
		return err
	}

	p1, err := ParseCaves(&b1)
	if err != nil {
		return err
	}
	s.p1 = p1

	p2, err := ParseCaves(&b2)
	if err != nil {
		return err
	}
	s.p2 = p2

	return nil
}

func (s *Solution) PartOne() (string, error) {
	return strconv.Itoa(s.p1.Start().Paths(1)), nil
}

func (s *Solution) PartTwo() (string, error) {
	return strconv.Itoa(s.p2.Start().Paths(2)), nil
}
