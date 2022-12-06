package day13

import (
	"fmt"
	"strings"
)

var mapping = map[int]string{
	15310472: "P",
	16312456: "F",
	10144425: "K",
	8947855:  "L",
	6916246:  "C",
}

func printchar(character [][]string) {
	for _, line := range character {
		fmt.Println(strings.Join(line, ""))
	}
}

func read(character [][]string) string {
	var id int
	for _, line := range character {
		for _, character := range line {
			id <<= 1

			if character == "#" {
				id |= 1
			}
		}
	}

	letter, ok := mapping[id]
	if !ok {
		printchar(character)
		panic(fmt.Sprintf("unknown character id: %v", id))
	}

	return letter
}
