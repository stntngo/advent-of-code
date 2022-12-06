package day15

import (
	"runtime/debug"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var testCase = `1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581`

func Test_OneFold(t *testing.T) {
	r := strings.NewReader(testCase)

	nodes, err := ParseNodes(r, 1)
	require.NoError(t, err)
	source := nodes[0]
	end := nodes[len(nodes)-1]
	path, err := FindPath(
		source,
		end,
		func(node Node) int {
			n := node.(*chiton)

			return (end.x - n.x) + (end.y - n.y)
		},
	)

	require.NoError(t, err)

	assert.Equal(t, 40, Risk(nodes, path))

}

func Test_FiveFold(t *testing.T) {
	r := strings.NewReader(testCase)

	nodes, err := ParseNodes(r, 5)
	require.NoError(t, err)
	source := nodes[0]
	end := nodes[len(nodes)-1]
	path, err := FindPath(
		source,
		end,
		func(node Node) int {
			n := node.(*chiton)

			return (end.x - n.x) + (end.y - n.y)
		},
	)

	require.NoError(t, err)

	assert.Equal(t, 315, Risk(nodes, path))

}

func Benchmark_FiveFold(b *testing.B) {
	debug.SetGCPercent(-1)
	r := strings.NewReader(testCase)

	nodes, err := ParseNodes(r, 5)
	require.NoError(b, err)
	source := nodes[0]
	end := nodes[len(nodes)-1]
	for i := 0; i < b.N; i++ {
		path, err := FindPath(
			source,
			end,
			func(node Node) int {
				n := node.(*chiton)

				return (end.x - n.x) + (end.y - n.y)
			},
		)

		require.NoError(b, err)
		assert.Equal(b, 315, Risk(nodes, path))
	}

}
