package day15

import (
	"bytes"
	"io"
	"strconv"
)

type Solution struct {
	first  map[int]*chiton
	second map[int]*chiton
}

func (s *Solution) Name() string {
	return "Chiton"
}

func (s *Solution) Load(r io.Reader) error {
	var b1, b2 bytes.Buffer
	w := io.MultiWriter(&b1, &b2)
	if _, err := io.Copy(w, r); err != nil {
		return err
	}

	first, err := ParseNodes(&b1, 1)
	if err != nil {
		return err
	}

	s.first = first

	second, err := ParseNodes(&b2, 5)
	if err != nil {
		return err
	}

	s.second = second
	return nil
}

func (s *Solution) PartOne() (string, error) {
	source := s.first[0]
	end := s.first[len(s.first)-1]
	path, err := FindPath(
		source,
		end,
		func(node Node) int {
			n := node.(*chiton)

			return (end.x - n.x) + (end.y - n.y)
		},
	)

	if err != nil {
		return "", err
	}
	score := Risk(s.first, path)
	return strconv.Itoa(score), nil
}

func (s *Solution) PartTwo() (string, error) {
	source := s.second[0]
	end := s.second[len(s.second)-1]
	path, err := FindPath(
		source,
		end,
		func(node Node) int {
			n := node.(*chiton)

			return (end.x - n.x) + (end.y - n.y)
		},
	)

	if err != nil {
		return "", err
	}

	score := Risk(s.second, path)
	return strconv.Itoa(score), nil
}
