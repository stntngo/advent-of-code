package day01

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var testCase = `199
200
208
210
200
207
240
269
260
263`

func Test_Parse(t *testing.T) {
	r := strings.NewReader(testCase)

	reading, err := ParseSonarReading(r)
	require.NoError(t, err)
	require.Len(t, reading, 10)
}

func Test_DepthCount(t *testing.T) {
	r := strings.NewReader(testCase)

	reading, err := ParseSonarReading(r)
	require.NoError(t, err)
	assert.Equal(t, 7, reading.DepthIncrease())
}

func Test_SlidingWindow(t *testing.T) {
	r := strings.NewReader(testCase)

	reading, err := ParseSonarReading(r)
	require.NoError(t, err)
	assert.Equal(t, 5, reading.SlidingWindow(3).DepthIncrease())

}
