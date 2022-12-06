package day15

import (
	"container/heap"
	"math"
)

type Distances map[ID]int

func (d Distances) Get(id ID) int {
	val, ok := d[id]
	if !ok {
		return math.MaxInt
	}

	return val
}

func (d Distances) Set(id ID, value int) {
	d[id] = value
}

type HeapItem struct {
	value    Node
	priority int
	index    int
}

type MinHeap []*HeapItem

func (mh MinHeap) Len() int { return len(mh) }

func (mh MinHeap) Less(i, j int) bool {
	return mh[i].priority < mh[j].priority
}

func (mh MinHeap) Swap(i, j int) {
	mh[i], mh[j] = mh[j], mh[i]
	mh[i].index = i
	mh[j].index = j
}

func (mh *MinHeap) Push(x interface{}) {
	n := len(*mh)
	item := x.(*HeapItem)
	item.index = n
	*mh = append(*mh, item)
}

func (mh *MinHeap) Pop() interface{} {
	old := *mh
	n := len(old)
	item := old[n-1]
	old[n-1] = nil
	item.index = -1
	*mh = old[0 : n-1]
	return item
}

func (mh *MinHeap) PopNode() Node {
	i := heap.Pop(mh)
	item := i.(*HeapItem)

	return item.value
}

func (mh *MinHeap) Update(node Node, priority int) {
	for _, item := range *mh {
		if item.value.ID() != node.ID() {
			continue
		}

		item.priority = priority
		heap.Fix(mh, item.index)
		return
	}

	item := &HeapItem{
		value:    node,
		index:    len(*mh),
		priority: priority,
	}

	heap.Push(mh, item)
}
