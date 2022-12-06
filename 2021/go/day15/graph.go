package day15

import (
	"errors"
)

type ID int

type Edge interface {
	From() Node
	To() Node
	Weight() int
}

type Node interface {
	ID() ID
	Edges() []Edge
}

func FindPath(source, end Node, heuristic func(Node) int) ([]ID, error) {
	dist := make(Distances)
	prev := make(map[ID]ID)
	var q MinHeap

	dist.Set(source.ID(), 0)
	q.Update(source, 0)

	for q.Len() > 0 {

		cur := q.PopNode()
		if cur.ID() == end.ID() {
			id := cur.ID()

			var path []ID
			for id != source.ID() {
				path = append(path, id)
				id = prev[id]
			}

			return path, nil
		}

		for _, edge := range cur.Edges() {
			val := dist.Get(cur.ID()) + edge.Weight()
			target := edge.To()
			if dist.Get(cur.ID()) == 0 || val < dist.Get(target.ID()) {
				prev[target.ID()] = cur.ID()
				dist.Set(target.ID(), val)

				est := val + heuristic(target)
				q.Update(target, est)
			}
		}
	}

	return nil, errors.New("impossible path")
}
