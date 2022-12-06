package day03

import (
	"bufio"
	"errors"
	"io"
	"strconv"
	"strings"
)

func BitArrayToInt(ba []string) (uint64, error) {
	return strconv.ParseUint(strings.Join(ba, ""), 2, 64)
}

func Parse(r io.Reader) ([][]string, error) {
	scanner := bufio.NewScanner(r)

	var lines [][]string
	for scanner.Scan() {
		lines = append(lines, strings.Split(scanner.Text(), ""))
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return lines, nil
}

func Transpose(lines [][]string) [][]string {
	out := make([][]string, 0, len(lines[0]))

	for i := 0; i < len(lines[0]); i++ {
		line := make([]string, 0, len(lines))
		for _, row := range lines {
			line = append(line, row[i])
		}

		out = append(out, line)
	}

	return out
}

func Count(bits []string) (int, int) {
	var zeroes, ones int
	for _, bit := range bits {
		switch bit {
		case "0":
			zeroes++
		case "1":
			ones++
		default:
			panic("unrecognized bit")
		}
	}

	return zeroes, ones
}

func PowerConsumption(lines [][]string) (uint64, error) {
	lines = Transpose(lines)

	var gamma, epsilon uint64
	for _, line := range lines {
		zeroes, ones := Count(line)

		// Fun with bit manipulation. After counting the number of zeroes
		// and ones in the line, we can directly manipulate the gamma
		// and epsilon values pretty straightforwardly and avoid the
		// BitArray that we make use of in the second part of the problem.
		//
		// First we shift both gamma and epsilon to the left one place.
		//
		// Example: gamma == 0b0001
		//
		//   0b0001
		// <<     1
		// --------
		//   0b0010
		gamma <<= 1
		epsilon <<= 1
		if ones > zeroes {
			// Using the same example as above we perform a bitwise
			// or on gamma when there are more zeroes than there
			// are ones.
			//
			// Example: gamma == 0b0010
			//
			//  0b0010
			// |0b0001
			// -------
			//  0b0010
			gamma |= 1
		} else {
			epsilon |= 1
		}

	}

	return gamma * epsilon, nil
}

// BitFilter lacks the neatness of the bitwise calculation of gamma and epsilon
// -- although it remains totally possible to use bitmasks to solve this problem --
// but it still has a tidy recursive property to it.
//
// Given our input set of numbers, we can iteratively filter out the numbers whose
// bits at the specified index do not match the value returned by our target function.
// With this reduced search space we can then recursively call BitFilter again stepping
// through to the next idx until only one number remains at which point we can return
// that number and it can be converted from an array of unicode "bits" into a proper
// integer.
func BitFilter(numbers [][]string, idx int, tgt func(int, int) string) ([]string, error) {
	if len(numbers) == 0 {
		return nil, errors.New("no numbers left to filter")
	}

	if len(numbers) == 1 {
		return numbers[0], nil
	}

	target := tgt(Count(Transpose(numbers)[idx]))

	var candidates [][]string
	for _, number := range numbers {
		if number[idx] == target {
			candidates = append(candidates, number)
		}
	}

	return BitFilter(candidates, idx+1, tgt)
}

func LifeSupport(lines [][]string) (uint64, error) {
	oxygenRaw, err := BitFilter(lines, 0, func(z, o int) string {
		if o >= z {
			return "1"
		} else {
			return "0"
		}
	})
	if err != nil {
		return 0, err
	}

	carbonRaw, err := BitFilter(lines, 0, func(z, o int) string {
		if o < z {
			return "1"
		} else {
			return "0"
		}
	})
	if err != nil {
		return 0, err
	}

	oxygen, err := BitArrayToInt(oxygenRaw)
	if err != nil {
		return 0, err
	}

	carbon, err := BitArrayToInt(carbonRaw)
	if err != nil {
		return 0, err
	}

	return oxygen * carbon, nil
}
