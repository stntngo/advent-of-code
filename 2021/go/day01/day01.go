package day01

import (
	"bufio"
	"io"
	"strconv"
)

// SonarReading is just a slice of Integers. I've made a type alias for it
// really just to hang the DepthIncrease and SlidingWindow methods off
// of for personal ergonomics. We could just as well not define SonarReading
// and instead make DepthIncrease a func([]int) int and make SlidingWindow
// a func([]int, int) []int.
type SonarReading []int

func (r SonarReading) DepthIncrease() int {
	var count int
	for i := 1; i < len(r); i++ {
		if r[i-1] < r[i] {
			count++
		}
	}

	return count
}

// SlidingWindow has a nice property to that leads to DepthIncrase code
// reuse. Since the problem has us sum the window of size 3, that is take
// a []int -> int, we're ultimately just left with a new SonarReading
// and therefore we can neatly chain calls to SlidingWindow and DepthIncrease
// to any arbitrary level of complexity, even if we wanted to something like
// reaidng.SlidingWindow(3).SlidingWindow(2).DepthIncrease() that's totally
// fine.
func (r SonarReading) SlidingWindow(size int) SonarReading {
	var out []int
	for i := size - 1; i < len(r); i++ {
		var value int
		for j := 0; j < size; j++ {
			value += r[i-j]
		}

		out = append(out, value)
	}

	return out
}

// ParseSonarReading isn't all too interesting, but it does set
// the stage for a lot the future parse functions we'll have
// throughout Advent of Code. We accept an io.Reader to accept
// the minimal surface area we require to process the input.
// This let's us make use of all sorts of inputs without changing
// our Parser, we can use a string with strings.Reader, a *os.File,
// an fs.File, or even os.Stdin.
func ParseSonarReading(r io.Reader) (SonarReading, error) {
	scanner := bufio.NewScanner(r)

	var reading []int
	for scanner.Scan() {
		i, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		}

		reading = append(reading, i)
	}

	// Easy to forget to check the error on the scanner, stamping the
	// scope of the error value returned here with err := scanner.Err()
	// instead dof something like:
	//
	// if scanner.Err() != nil { return scanner.Err() }
	//
	// lets us be absolutely certain that the value you return is
	// the value we checked against. Imagine a bug in which a subsequent
	// call to scanner.Err() -- for whatever reason --  didn't return
	// the error you just checked against.
	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return reading, nil
}
