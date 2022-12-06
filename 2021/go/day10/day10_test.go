package day10

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var testCase = `[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]`

func Test_Score(t *testing.T) {
	r := strings.NewReader(testCase)

	lines, err := Lines(r)
	require.NoError(t, err)

	corruption, autocomplete := Score(lines)

	assert.Equal(t, 26397, corruption)
	assert.Equal(t, 288957, autocomplete)
}
