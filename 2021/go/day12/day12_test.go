package day12

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var (
	example = `start-A
start-b
A-c
A-b
b-d
A-end
b-end`
	slightlyLarger = `dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc`
	evenLarger = `fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW`
)

func Test_ExamplePart1(t *testing.T) {
	r := strings.NewReader(example)

	caves, err := ParseCaves(r)
	require.NoError(t, err)
	assert.Equal(t, 10, caves.Start().Paths(1))
}

func Test_ExamplePart2(t *testing.T) {
	r := strings.NewReader(example)

	caves, err := ParseCaves(r)
	require.NoError(t, err)
	assert.Equal(t, 36, caves.Start().Paths(2))
}

func Test_SlightyLargerPart1(t *testing.T) {
	r := strings.NewReader(slightlyLarger)

	caves, err := ParseCaves(r)
	require.NoError(t, err)
	assert.Equal(t, 19, caves.Start().Paths(1))
}

func Test_SlightlyLargerPart2(t *testing.T) {
	r := strings.NewReader(slightlyLarger)

	caves, err := ParseCaves(r)
	require.NoError(t, err)
	assert.Equal(t, 103, caves.Start().Paths(2))
}

func Test_EvenLargerPart1(t *testing.T) {
	r := strings.NewReader(evenLarger)

	caves, err := ParseCaves(r)
	require.NoError(t, err)
	assert.Equal(t, 226, caves.Start().Paths(1))
}

func Test_EvenLargerPart2(t *testing.T) {
	r := strings.NewReader(evenLarger)

	caves, err := ParseCaves(r)
	require.NoError(t, err)
	assert.Equal(t, 3509, caves.Start().Paths(2))
}
