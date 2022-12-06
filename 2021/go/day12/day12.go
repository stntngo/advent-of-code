package day12

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"sort"
	"strings"
	"unicode"
)

type Visit struct {
	ID    string
	Count int
}

type CaveSystem []*Cave

func (c *CaveSystem) Start() *Cave {
	for _, cave := range *c {
		if cave.Kind == Start {
			return cave
		}
	}

	panic("unreachable")
}

func (c *CaveSystem) Add(cave *Cave) {
	*c = append(*c, cave)
}

func (c CaveSystem) Find(id string) (*Cave, bool) {
	for _, cave := range c {
		if cave.Name == id {
			return cave, true
		}
	}

	return nil, false
}

type CaveType int

const (
	Small CaveType = iota + 1
	Large
	Start
	End
)

type Counter struct {
	ID    string
	Count int
}

func VisitHash(visited []*Cave) string {
	count := make([]*Counter, 0, len(visited))

	for _, cave := range visited {
		if cave.Kind == Large {
			continue
		}

		var found bool
		for _, existing := range count {
			if existing.ID == cave.Name {
				existing.Count++
				found = true
			}
		}

		if !found {
			count = append(
				count,
				&Counter{
					ID:    cave.Name,
					Count: 1,
				},
			)
		}
	}

	sort.Slice(count, func(i, j int) bool {
		return count[i].ID < count[j].ID
	})

	var out string
	for _, counter := range count {
		out += fmt.Sprintf("%v:%v//", counter.ID, counter.Count)
	}

	return out
}

type Cave struct {
	Name string
	Kind CaveType

	Connections []*Cave
	paths       map[string]int
}

func (c *Cave) Paths(max int, visited ...*Cave) (paths int) {
	hash := VisitHash(visited)
	if count, ok := c.paths[hash]; ok {
		return count
	}

	defer func() {
		c.paths[hash] = paths
	}()

	seen := make(map[string]int)
	for _, v := range visited {
		if v.Kind != Large {
			seen[v.Name]++
		}
	}

	switch c.Kind {
	case Start:
		if seen[c.Name] == 1 {
			return 0
		}
	case End:
		if seen[c.Name] == 1 {
			return 0
		}
	case Small:
		if seen[c.Name] == max {
			return 0
		}
	}

	var atmax int
	for _, count := range seen {
		if count == max && max > 1 {
			atmax++
		}
	}

	if atmax > 1 {
		return 0
	}

	if c.Kind == End {
		return 1
	}

	visited = append([]*Cave{c}, visited...)

	for _, cxn := range c.Connections {
		paths += cxn.Paths(max, visited...)
	}

	return paths
}

func NewCave(id string) *Cave {
	ctype := Large

	switch id {
	case "start":
		ctype = Start
	case "end":
		ctype = End
	default:
		for _, c := range id {
			if unicode.IsLower(c) {
				ctype = Small
				break
			}

		}
	}

	return &Cave{
		Name:        id,
		Kind:        ctype,
		Connections: make([]*Cave, 0),
		paths:       make(map[string]int),
	}
}

func ParseCaves(r io.Reader) (CaveSystem, error) {
	scanner := bufio.NewScanner(r)

	var system CaveSystem
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), "-")
		if len(parts) != 2 {
			return nil, errors.New("expected format [cave]-[cave]")
		}

		a, ok := system.Find(parts[0])
		if !ok {
			a = NewCave(parts[0])
			system.Add(a)
		}

		b, ok := system.Find(parts[1])
		if !ok {
			b = NewCave(parts[1])
			system.Add(b)
		}

		a.Connections = append(a.Connections, b)
		b.Connections = append(b.Connections, a)

	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return system, nil
}
