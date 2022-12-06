package day02

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var testCase = `forward 5
down 5
forward 8
up 3
down 8
forward 2`

func Test_ParseCommands(t *testing.T) {
	r := strings.NewReader(testCase)

	commands, err := ParseCommands(r)
	require.NoError(t, err)

	assert.Len(t, commands, 6)
}

func Test_Vector(t *testing.T) {
	r := strings.NewReader(testCase)

	commands, err := ParseCommands(r)
	require.NoError(t, err)

	vec := Vector(commands)
	assert.Equal(t, 150, vec)
}

func Test_AimVector(t *testing.T) {
	r := strings.NewReader(testCase)

	commands, err := ParseCommands(r)
	require.NoError(t, err)

	vec := AimVector(commands)
	assert.Equal(t, 900, vec)
}
