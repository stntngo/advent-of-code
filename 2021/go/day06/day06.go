package day06

import (
	"strconv"
	"strings"
)

// We can model the population of lantern fish as an array of nine integers.
// Each index is associated with the subpopulation of lantern fish with a
// timer value equal to that index.
//
// If the first element of the LanternFish array is 10 that means there are
// 10 lantern fish in the population with a "0" timer who will reproduce
// at the next opportunity.
type LanternFish [9]uint64

func (l LanternFish) Pop() uint64 {
	var sum uint64

	for _, fish := range l {
		sum += fish
	}

	return sum
}

func SimulatePopulation(population LanternFish, days int) LanternFish {
	for day := 0; day < days; day++ {
		var next LanternFish

		// Each day the population of lantern fish will cycle around
		// the array, index 8 will move to index 7, index 7 will move to
		// index 6, index 6 will move to index 5 and so on down to
		// index 0 which will wrap around back to index 8 (the
		// newly born lantern fish).
		for i := 0; i < 9; i++ {
			x := (i + 8) % 9
			next[x] = population[i]
		}

		// The same size population that just "birthed" new lantern
		// fish with a timer value of 8 will aslo have their individual
		// timers' reset to 6.
		next[6] += next[8]

		population = next
	}

	return population
}

func Parse(s string) (LanternFish, error) {
	var lanternfish LanternFish
	for _, num := range strings.Split(s, ",") {
		fish, err := strconv.Atoi(num)
		if err != nil {
			return lanternfish, err
		}

		lanternfish[fish]++
	}

	return lanternfish, nil
}
