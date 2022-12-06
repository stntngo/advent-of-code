package day14

import (
	"bufio"
	"errors"
	"io"
	"strings"
)

type Insertion struct {
	input  string
	output []string
}

func ParseInsertion(s string) (Insertion, error) {
	parts := strings.Split(s, " -> ")
	if len(parts) != 2 {
		return Insertion{}, errors.New("unexpected insertion rule")
	}

	return Insertion{
		input: parts[0],
		output: []string{
			string(parts[0][:1]) + parts[1],
			parts[1] + string(parts[0][1:]),
		},
	}, nil
}

func ParsePolymers(r io.Reader) (string, []Insertion, error) {
	scanner := bufio.NewScanner(r)

	var lines []string
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}

		lines = append(lines, line)
	}

	template, rawRules := lines[0], lines[1:]

	rules := make([]Insertion, len(rawRules))
	for i, raw := range rawRules {
		rule, err := ParseInsertion(raw)
		if err != nil {
			return "", nil, err
		}

		rules[i] = rule
	}

	return template, rules, nil
}

func ExtendPolymer(template string, rules []Insertion, rounds int) map[string]uint64 {
	counter := make(map[string]uint64)

	for _, pair := range Window(template, 2) {
		counter[pair]++
	}

	for i := 0; i < rounds; i++ {
		next := make(map[string]uint64)

		for pair, count := range counter {
			for _, rule := range rules {
				if rule.input == pair {
					for _, translation := range rule.output {
						next[translation] += count
					}

					break
				}
			}
		}

		counter = next
	}

	return counter
}

func Window(template string, size int) []string {
	var out []string
	for i := size - 1; i < len(template); i++ {
		out = append(out, template[i-size+1:i+1])
	}

	return out

}

func PolymerScore(initial string, polymer map[string]uint64) uint64 {
	counter := make(map[string]uint64)

	for pair, count := range polymer {
		terms := strings.Split(pair, "")
		counter[terms[1]] += count
	}

	first := strings.Split(initial, "")[0]
	counter[first]++

	var max, min uint64
	for _, value := range counter {
		if max == 0 || value > max {
			max = value
		}

		if min == 0 || value < min {
			min = value
		}
	}

	return max - min
}
