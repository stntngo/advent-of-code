package day13

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var (
	testCase = `6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5`
	expected = [][]string{
		{"#", "#", "#", "#", "#"},
		{"#", " ", " ", " ", "#"},
		{"#", " ", " ", " ", "#"},
		{"#", " ", " ", " ", "#"},
		{"#", "#", "#", "#", "#"},
	}
)

func Test_Day13(t *testing.T) {
	grid, inst, err := ParsePoints(strings.NewReader(testCase))
	require.NoError(t, err)

	assert.Len(t, Fold(grid, inst[:1]), 17)
	assert.Equal(t, expected, Fold(grid, inst).Lines())
}
