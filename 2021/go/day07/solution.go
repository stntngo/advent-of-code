package day07

import (
	"io"
	"io/ioutil"
	"strconv"
)

type Solution struct {
	nums []int
}

func (s *Solution) Name() string {
	return "The Treachery of Whales"
}

func (s *Solution) Load(r io.Reader) error {
	b, err := ioutil.ReadAll(r)
	if err != nil {
		return err
	}

	nums, err := ParseNums(string(b))
	if err != nil {
		return err
	}

	s.nums = nums

	return nil
}

func (s *Solution) PartOne() (string, error) {
	mid := MidPoint(s.nums)

	return strconv.Itoa(int(SillyLineSearch(L1(s.nums), mid, 256))), nil
}

func (s *Solution) PartTwo() (string, error) {
	mid := MidPoint(s.nums)

	return strconv.Itoa(int(SillyLineSearch(L2(s.nums), mid, 256))), nil
}
