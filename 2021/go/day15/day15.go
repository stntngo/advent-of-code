package day15

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

type edge struct {
	from, to *chiton
}

func (e edge) From() Node {
	return e.from
}

func (e edge) To() Node {
	return e.to
}

func (e edge) Weight() int {
	return e.to.risk
}

type chiton struct {
	id, risk, x, y int
	connections    []*chiton
}

func (c *chiton) ID() ID {
	return ID(c.id)
}

func (c *chiton) Edges() []Edge {
	edges := make([]Edge, len(c.connections))

	for i, conn := range c.connections {
		edges[i] = &edge{
			from: c,
			to:   conn,
		}
	}

	return edges
}

func Risk(nodes map[int]*chiton, path []ID) int {
	var risk int
	for _, id := range path {
		risk += nodes[int(id)].risk
	}

	return risk
}

func ParseNodes(r io.Reader, reps int) (map[int]*chiton, error) {
	scanner := bufio.NewScanner(r)

	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	var rows [][]*chiton
	var id int

	var yi int
	for y := 0; y < reps; y++ {
		for _, line := range lines {
			var row []*chiton
			var xi int
			for x := 0; x < reps; x++ {
				for _, num := range strings.Split(line, "") {
					risk, err := strconv.Atoi(num)
					if err != nil {
						return nil, err
					}

					risk = (risk + x + y)

					if risk >= 10 {
						risk = risk%10 + 1
					}

					row = append(
						row,
						&chiton{
							id:          id,
							risk:        risk,
							x:           xi,
							y:           yi,
							connections: make([]*chiton, 0),
						},
					)

					id++
					xi++
				}
			}

			rows = append(rows, row)
			yi++
		}
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	out := make(map[int]*chiton)
	for y, row := range rows {
		for x, node := range row {
			for _, yadj := range []int{-1, 1} {
				ny := y + yadj

				if ny < 0 || ny >= len(rows) {
					continue
				}

				node.connections = append(node.connections, rows[ny][x])
			}

			for _, xadj := range []int{-1, 1} {
				nx := x + xadj

				if nx < 0 || nx >= len(row) {
					continue
				}

				node.connections = append(node.connections, rows[y][nx])
			}

			out[node.id] = node
		}
	}

	return out, nil
}
