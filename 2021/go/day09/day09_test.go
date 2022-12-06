package day09

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var testCase = `2199943210
3987894921
9856789892
8767896789
9899965678`

func Test_RiskLevel(t *testing.T) {
	hm, err := Parse(strings.NewReader(testCase))
	require.NoError(t, err)

	assert.Equal(t, 15, hm.RiskLevel())

}
func Test_BasinFactor(t *testing.T) {
	hm, err := Parse(strings.NewReader(testCase))
	require.NoError(t, err)

	assert.Equal(t, 1134, hm.BasinFactor())
}
