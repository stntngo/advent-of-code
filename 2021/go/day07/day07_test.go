package day07

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var testCase = `16,1,2,0,4,2,7,1,2,14`

func Test_Optimizer(t *testing.T) {
	nums, err := ParseNums(testCase)
	require.NoError(t, err)

	mid := MidPoint(nums)
	assert.Equal(t, 37, int(SillyLineSearch(L1(nums), mid, 256)))
	assert.Equal(t, 168, int(SillyLineSearch(L2(nums), mid, 256)))
}
