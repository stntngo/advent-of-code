package day06

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func Test_LanternFish(t *testing.T) {
	fish, err := Parse("3,4,3,1,2")
	require.NoError(t, err)
	assert.Equal(t, uint64(5934), SimulatePopulation(fish, 80).Pop())
	assert.Equal(t, uint64(26984457539), SimulatePopulation(fish, 256).Pop())
}
