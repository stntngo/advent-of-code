package day10

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"sort"
	"strings"
)

type ErrCorruptedLine struct {
	Expected, Got string
}

func (e *ErrCorruptedLine) Error() string {
	return fmt.Sprintf("corrupt line: expected %q got %q", e.Expected, e.Got)
}

type ErrUnterminatedLine struct {
	Remaining []string
}

func (e *ErrUnterminatedLine) Error() string {
	return fmt.Sprintf("unterminated line: %s remaining", strings.Join(e.Remaining, ""))
}

// ParseLine uses custom errors defined above to communicate
// extra information to our function callers in order to provide
// a clean "ok / not ok interface". Even though the problem itself
// will never provide you with a balanced line, we can write our
// code to handle that case, returning a nil error if we're
// able to parse the entire line without any symbols remaining
// in our terminator stack.
func ParseLine(s string) error {
	var stack []string
	for _, sym := range strings.Split(s, "") {
		switch sym {
		case "(":
			stack = append(stack, ")")
		case "[":
			stack = append(stack, "]")
		case "{":
			stack = append(stack, "}")
		case "<":
			stack = append(stack, ">")
		default:
			n := len(stack) - 1

			var term string
			term, stack = stack[n], stack[:n]

			if term != sym {
				return &ErrCorruptedLine{
					Expected: term,
					Got:      sym,
				}
			}
		}
	}

	if len(stack) > 1 {
		return &ErrUnterminatedLine{
			Remaining: stack,
		}
	}

	return nil
}

func Score(f []string) (int, int) {
	var corruptionScore int
	var autocompleteScores []int

	for _, line := range f {
		if err := ParseLine(line); err != nil {
			// Since we've encoded two different
			// kinds of failure within the
			// two error types we return we can
			// use errors.As to "convert" our returned
			// error into the specific error we're looking
			// for. If we can't convert our error into
			// the error we're looking for, that's not
			// a problem, as errors.As will return false,
			// and we won't try to access any of the
			// extra data inside of the specific error.
			var corruption *ErrCorruptedLine
			if errors.As(err, &corruption) {
				switch corruption.Got {
				case ")":
					corruptionScore += 3
				case "]":
					corruptionScore += 57
				case "}":
					corruptionScore += 1197
				case ">":
					corruptionScore += 25137
				default:
					panic("unknown terminator")
				}
			}

			// You'll oftentimes also see responding
			// to specific error types that looks something
			// like this.
			//
			// switch v := err.(type) {
			// case *ErrCorruptedLine:
			// 	// Handle v.Expected and v.Got
			// case *ErrUnterminatedLine:
			// 	// Handle v.Remaining
			// default:
			// 	return err
			// }
			//
			// This approach might initially appear like the
			// better option in our use case because you can
			// more concisely respond and react to the multiple
			// kinds of errors it returns. The issue with the
			// type switch statement like this is the fact that
			// errors.As will handle wrapped errors for us, while
			// the type switch statement will not. And we've already
			// mentioned on an earlier day the utility that wrapped
			// errors can bring us.
			var incomplete *ErrUnterminatedLine
			if errors.As(err, &incomplete) {
				var score int
				for i := len(incomplete.Remaining) - 1; i >= 0; i-- {
					score *= 5
					switch incomplete.Remaining[i] {
					case ")":
						score += 1
					case "]":
						score += 2
					case "}":
						score += 3
					case ">":
						score += 4
					default:
						panic("unknown terminator")
					}
				}

				autocompleteScores = append(autocompleteScores, score)
			}
		}
	}

	sort.Ints(autocompleteScores)
	return corruptionScore, autocompleteScores[len(autocompleteScores)/2]
}

func Lines(r io.Reader) ([]string, error) {
	scanner := bufio.NewScanner(r)

	var out []string
	for scanner.Scan() {
		out = append(out, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return out, nil
}
