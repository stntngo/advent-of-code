package day09

import (
	"bufio"
	"io"
	"sort"
	"strconv"
	"strings"
)

type HeightNode struct {
	Coordinate
	Value int

	Inflows []*HeightNode
}

type NodeMap [][]*HeightNode

func ConvertNodeMap(hm HeightMap) NodeMap {
	nm := make(NodeMap, 0, len(hm))

	for y, row := range hm {
		nmr := make([]*HeightNode, 0, len(row))

		for x, value := range row {
			node := &HeightNode{
				Coordinate: Coordinate{
					X: x,
					Y: y,
				},
				Value: value,
			}

			nmr = append(nmr, node)
		}

		nm = append(nm, nmr)
	}

	for _, row := range nm {
		for _, node := range row {
			for _, coords := range hm.Neighbors(node.Coordinate) {
				neighbor := nm[coords.Y][coords.X]

				if neighbor.Value == 9 {
					continue
				}

				if neighbor.Value > node.Value {
					node.Inflows = append(node.Inflows, neighbor)
				}

			}
		}
	}

	return nm
}

type Coordinate struct {
	X, Y int
}

type HeightMap [][]int

func (h HeightMap) Neighbors(coord Coordinate) []Coordinate {
	var neighbors []Coordinate

	switch coord.X {
	case 0:
		neighbors = append(neighbors, Coordinate{coord.X + 1, coord.Y})
	case len(h[0]) - 1:
		neighbors = append(neighbors, Coordinate{coord.X - 1, coord.Y})
	default:
		neighbors = append(neighbors, Coordinate{coord.X - 1, coord.Y}, Coordinate{coord.X + 1, coord.Y})
	}

	switch coord.Y {
	case 0:
		neighbors = append(neighbors, Coordinate{coord.X, coord.Y + 1})
	case len(h) - 1:
		neighbors = append(neighbors, Coordinate{coord.X, coord.Y - 1})
	default:
		neighbors = append(neighbors, Coordinate{coord.X, coord.Y - 1}, Coordinate{coord.X, coord.Y + 1})
	}

	return neighbors
}

func (h HeightMap) IsLowPoint(coord Coordinate) bool {
	value := h[coord.Y][coord.X]
	for _, neighbor := range h.Neighbors(coord) {
		if h[neighbor.Y][neighbor.X] <= value {
			return false
		}

	}

	return true
}

func (hm HeightMap) LowPoints() []*HeightNode {
	nm := ConvertNodeMap(hm)

	var out []*HeightNode
	for y, row := range hm {
		for x := range row {
			if hm.IsLowPoint(Coordinate{x, y}) {
				out = append(out, nm[y][x])
			}
		}
	}

	return out

}

func (hm HeightMap) Basins() []map[Coordinate]bool {
	var out []map[Coordinate]bool
	for _, node := range hm.LowPoints() {
		basin := make(map[Coordinate]bool)

		basin[node.Coordinate] = true

		queue := make([]*HeightNode, len(node.Inflows))
		copy(queue, node.Inflows)

		for len(queue) > 0 {
			node, queue = queue[0], queue[1:]

			basin[node.Coordinate] = true

			queue = append(queue, node.Inflows...)
		}

		out = append(out, basin)
	}

	return out
}

func (hm HeightMap) RiskLevel() int {
	var level int
	for _, low := range hm.LowPoints() {
		level += low.Value + 1
	}

	return level

}

func (hm HeightMap) BasinFactor() int {
	basins := hm.Basins()

	sizes := make([]int, 0, len(basins))

	for _, basin := range basins {
		sizes = append(sizes, len(basin))
	}

	sort.Ints(sizes)

	return sizes[len(sizes)-1] * sizes[len(sizes)-2] * sizes[len(sizes)-3]
}

func Parse(r io.Reader) (HeightMap, error) {
	scanner := bufio.NewScanner(r)

	var h HeightMap

	for scanner.Scan() {
		line := scanner.Text()

		row := make([]int, 0, len(line))
		for _, n := range strings.Split(line, "") {
			height, err := strconv.Atoi(n)
			if err != nil {
				return nil, err
			}

			row = append(row, height)
		}

		h = append(h, row)
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return h, nil
}
