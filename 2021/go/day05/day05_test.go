package day05

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var testCase = `0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2`

func Test_VentCounting(t *testing.T) {
	r := strings.NewReader(testCase)

	lines, err := Parse(r)
	require.NoError(t, err)

	noDiagonals := make([]Line, 0, len(lines))

	for _, line := range lines {
		if line.LineType() != Diagonal {
			noDiagonals = append(noDiagonals, line)
		}
	}

	assert.Equal(t, 5, CountHotSpots(noDiagonals))
	assert.Equal(t, 12, CountHotSpots(lines))
}
