package day03

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var testCase = `00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010`

func Test_PowerConsumption(t *testing.T) {
	r := strings.NewReader(testCase)

	lines, err := Parse(r)
	require.NoError(t, err)

	power, err := PowerConsumption(lines)
	require.NoError(t, err)
	assert.Equal(t, uint64(198), power)
}

func Test_LifeSupport(t *testing.T) {
	r := strings.NewReader(testCase)

	lines, err := Parse(r)
	require.NoError(t, err)

	life, err := LifeSupport(lines)
	require.NoError(t, err)
	assert.Equal(t, uint64(230), life)
}
