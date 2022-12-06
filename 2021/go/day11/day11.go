package day11

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

const _SIZE = 10

type Coordinate struct {
	x, y int
}

func (c Coordinate) Neighbors() []Coordinate {
	neighbors := make([]Coordinate, 0, 8)
	for dx := -1; dx <= 1; dx++ {
		for dy := -1; dy <= 1; dy++ {
			if dx == 0 && dy == 0 {
				continue
			}

			nx := c.x + dx
			ny := c.y + dy

			if nx < 0 || ny < 0 {
				continue
			}

			if nx >= _SIZE || ny >= _SIZE {
				continue
			}

			neighbors = append(neighbors, Coordinate{nx, ny})
		}
	}

	return neighbors
}

type Cavern [_SIZE][_SIZE]int

func (c *Cavern) Copy() Cavern {
	var cavern Cavern

	for y := 0; y < _SIZE; y++ {
		for x := 0; x < _SIZE; x++ {
			cavern[y][x] = c[y][x]
		}
	}

	return cavern
}

func (c *Cavern) Step() int {
	flashed := make(map[Coordinate]bool)
	lastRound := make([]Coordinate, 0, _SIZE*_SIZE)
	for y := 0; y < _SIZE; y++ {
		for x := 0; x < _SIZE; x++ {
			value := c[y][x] + 1
			c[y][x] = value

			if value > 9 {
				coord := Coordinate{x, y}
				lastRound = append(lastRound, coord)
				flashed[coord] = true
			}

		}
	}

	for len(lastRound) > 0 {
		nextRound := make([]Coordinate, 0, _SIZE*_SIZE)

		for _, coord := range lastRound {
			for _, neighbor := range coord.Neighbors() {
				value := c[neighbor.y][neighbor.x] + 1
				c[neighbor.y][neighbor.x] = value

				if value > 9 {
					if _, ok := flashed[neighbor]; ok {
						continue
					}

					nextRound = append(nextRound, neighbor)
					flashed[neighbor] = true
				}
			}
		}

		lastRound = nextRound
	}

	for y := 0; y < _SIZE; y++ {
		for x := 0; x < _SIZE; x++ {
			value := c[y][x]
			if value > 9 {
				c[y][x] = 0
			}
		}
	}

	return len(flashed)
}

func ParseCavern(r io.Reader) (Cavern, error) {
	scanner := bufio.NewScanner(r)

	var rows Cavern
	var rowIdx int
	for scanner.Scan() {
		line := scanner.Text()
		var row [_SIZE]int
		for i, str := range strings.Split(line, "") {
			num, err := strconv.Atoi(str)
			if err != nil {
				return rows, err
			}

			row[i] = num
		}

		rows[rowIdx] = row
		rowIdx++
	}

	if err := scanner.Err(); err != nil {
		return rows, err
	}

	return rows, nil
}
