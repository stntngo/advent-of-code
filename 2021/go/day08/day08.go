package day08

import (
	"bufio"
	"io"
	"math"
	"strings"
)

var (
	_COUNTS = []int{8, 6, 8, 7, 4, 9, 7}
	_DIGITS = [][]int{
		{0, 1, 2, 4, 5, 6},    // 0
		{2, 5},                // 1
		{0, 2, 3, 4, 6},       // 2
		{0, 2, 3, 5, 6},       // 3
		{1, 2, 3, 5},          // 4
		{0, 1, 3, 5, 6},       // 5
		{0, 1, 3, 4, 5, 6},    // 6
		{0, 2, 5},             // 7
		{0, 1, 2, 3, 4, 5, 6}, // 8
		{0, 1, 2, 3, 5, 6},    // 9
	}
	_COMBINATIONS = make([][]rune, 0, 5040)
)

func init() {
	// Broadly speaking there are two ways to solve Day 8, the "clever" way
	// and the brute force way. I implemented both the "clever" solution and
	// the brute force solution in Clojure. Here in Go however, I implemented
	// only the brute force method.
	//
	// I chose to do this for a couple reasons. Go wants you to do things
	// "the dumb way". It doesn't have fancy Higher Kinded Types, or generic
	// Map/Filter/Reduce functions out of a conscious decision to "keep it
	// simple (stupid)". Searching through every possible permutation of the
	// letters abcdefg, _is_  the simple way. Yes, it's more work for the
	// machine, but it's a neglibile performance hit in a compiled language
	// like Go. Because we identified that we can reuse the same 5,040
	// permutations for every attempt, we eat the up front cost of computing
	// all these permutations when the process starts and imports this module.
	// and as a result searching through all of these combinations only takes
	// around 35 milliseconds on my laptop.
	Permutations([]rune("abcdefg"), func(a []rune) {
		// We're mutating the `a` rune slice in place
		// here, so once we have successfulyl constructed
		// a valid permutation, we need to copy over
		// the `a` slice into the `b` slice before pushing it
		// into our _COMBINATIONS slice, or else we'd end
		// up with 5,040 instances of the same arrangement
		// of runes.
		b := make([]rune, len(a))
		copy(b, a)
		_COMBINATIONS = append(_COMBINATIONS, b)
	})
}

func Permutations(a []rune, f func([]rune)) {
	perm(a, f, 0)
}

func perm(a []rune, f func([]rune), i int) {
	if i > len(a) {
		f(a)
		return
	}
	perm(a, f, i+1)
	for j := i + 1; j < len(a); j++ {
		a[i], a[j] = a[j], a[i]
		perm(a, f, i+1)
		a[i], a[j] = a[j], a[i]
	}
}

func DigitSegments(pattern []rune) [][]rune {
	segments := make([][]rune, 0, 10)

	for _, d := range _DIGITS {
		digit := make([]rune, len(d))
		for i, j := range d {
			digit[i] = pattern[j]
		}

		segments = append(segments, digit)
	}

	return segments
}

func VerifySignal(segments []rune, signal Signal) bool {
	// But we shouldn't ignore _all_ possible optimizations.
	// Even though we're brute forcing our way through things
	// we can identify opportunities to cut down on repeatedly
	// trying the most expensive computations. By checking that,
	// at the very least, the segment frequencies would be accurate
	// when combining this segment configuration and signal,
	// we can reduce the time it takes to process all the potential
	// segment configurations over 7x.
	if !CountFilter(segments, signal) {
		return false
	}

	candidates := DigitSegments(segments)

	digits := make([]string, len(signal.digits))
	copy(digits, signal.digits)

	// A signal is valid if it's possible to map each candidate
	// digit to one and only one of the digits in the provided
	// signal.
	for _, candidate := range candidates {
		for i, digit := range digits {
			if SegmentMatch(candidate, digit) {
				// When we successfully map a digit
				// we remove that digit from the
				// list of candidates, and continue
				// on to the next candidate digit.
				digits[i] = digits[len(digits)-1]
				digits[len(digits)-1] = ""
				digits = digits[:len(digits)-1]

				break
			}
		}

	}

	// If we've mapped all the digits successfully, there will be nothing
	// left in the initial digits slice.
	return len(digits) == 0
}

type Signal struct {
	digits []string
	output []string
}

func (s Signal) Output() int {
	decoded := DecodeSignal(s)

	digits := DigitSegments(decoded)

	var final int
	for i, output := range s.output {
		d := DecodeOutput(digits, output)
		final += int(float64(d) * math.Pow(10, float64(len(s.output)-i-1)))
	}

	return final

}

func ParseSignal(s string) Signal {
	var signal Signal

	parts := strings.Split(s, " | ")
	for _, digit := range strings.Split(parts[0], " ") {
		signal.digits = append(signal.digits, digit)
	}

	for _, output := range strings.Split(parts[1], " ") {
		signal.output = append(signal.output, output)
	}

	return signal
}

func Parse(r io.Reader) ([]Signal, error) {
	scanner := bufio.NewScanner(r)

	var signals []Signal

	for scanner.Scan() {
		signal := ParseSignal(scanner.Text())

		signals = append(signals, signal)
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return signals, nil
}

func CountFilter(mapping []rune, signal Signal) bool {
	for i, c := range _COUNTS {

		var count int
		for _, digit := range signal.digits {
			if strings.ContainsRune(digit, mapping[i]) {
				count++
			}
		}

		if count != c {
			return false
		}
	}

	return true

}

func SegmentMatch(digit []rune, signal string) bool {
	if len(digit) != len(signal) {
		return false
	}

	for _, r := range digit {
		if !strings.ContainsRune(signal, r) {
			return false
		}
	}

	return true
}

func DecodeSignal(signal Signal) []rune {
	for _, candidate := range _COMBINATIONS {
		if VerifySignal(candidate, signal) {
			return candidate
		}
	}

	// These unreachable panics are actually
	// very bad practice. There's a _lot_ of
	// assumptions baked into their exsitence.
	// The only reason they can exist here at all
	// is because of the guarantees of the
	// Puzzle creator of advent of code.
	// In the real world, these functions would
	// 100% need to return ([]rune, error)
	// and not just a []rune.
	panic("unreachable")
}

func DecodeOutput(rules [][]rune, output string) int {
	for d, rule := range rules {
		if SegmentMatch(rule, output) {
			return d
		}
	}

	panic("unreachable")
}

func EasyDigitCount(signals []Signal) int {
	var count int
	for _, signal := range signals {
		for _, i := range []int{2, 3, 4, 7} {
			for _, output := range signal.output {
				if len(output) == i {
					count++
				}
			}

		}
	}

	return count
}

func SignalOutputSum(signals []Signal) int {
	var sum int
	for _, signal := range signals {
		sum += signal.Output()
	}

	return sum
}
