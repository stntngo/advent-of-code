package day04

import (
	"io"
	"strconv"
)

type Solution struct {
	rand   *RandomNumbers
	boards []Board
}

func (s *Solution) Name() string {
	return "Giant Squid"
}

func (s *Solution) Load(r io.Reader) error {
	rand, boards, err := Parse(r)
	if err != nil {
		return err
	}

	s.rand = rand
	s.boards = boards

	return nil
}

func (s *Solution) PartOne() (string, error) {
	winner, err := WinFirst(s.rand, s.boards)
	if err != nil {
		return "", err
	}

	return strconv.Itoa(s.rand.Score(winner)), nil
}

func (s *Solution) PartTwo() (string, error) {
	winner, err := WinLast(s.rand, s.boards)
	if err != nil {
		return "", err
	}

	return strconv.Itoa(s.rand.Score(winner)), nil
}
