package day04

import (
	"bufio"
	"errors"
	"io"
	"strconv"
	"strings"
)

func ParseRandomNumbers(line string) (*RandomNumbers, error) {
	strs := strings.Split(line, ",")
	nums := make([]int, len(strs))

	for i, str := range strs {
		num, err := strconv.Atoi(str)
		if err != nil {
			return nil, err
		}

		nums[i] = num
	}

	return &RandomNumbers{
		next:  nums,
		last:  -1,
		drawn: make(map[int]bool),
	}, nil
}

type RandomNumbers struct {
	next []int
	last int

	// See below for a longer comment on making use
	// of things like maps and slices to stand in
	// for data structures like sets and stacks below.
	//
	// Despite being a map[int]bool. This is being
	// treated as a set. The use of bool as the value
	// type here is actually redundant because Go's
	// map type already has the functionality to
	// return a bool representing whether a key is
	// present inside of the map or not. You might
	// often see a set declared as map[T]struct{}.
	// This is an empty struct. You can read more
	// about it here: https://dave.cheney.net/2014/03/25/the-empty-struct
	// And using it is a bit of a nice optimization
	// when you need to conserve memory because unlike
	// a bool, which is 1 byte in size, the empty
	// struct is 0 bytes in size and won't take up
	// any space.
	drawn map[int]bool
}

func (r *RandomNumbers) Draw() error {
	if len(r.next) < 1 {
		return errors.New("empty random number stream")
	}

	// Since Go lacks generics -- at least until 1.18 comes out --
	// a lot of data structures you might expect to exist in the
	// standard library don't. Things like sets, stacks, and queues
	// are treated as "you can accomplish that with a couple of duplicate
	// lines of code with either a map or a slice, just use those".
	// So while this might feel "wrong" because you would rather have some
	// type that actually has "pop" and "push" methods, this _is_ a stack
	// and this _is_ an ok way to set about doing it in Go.
	r.last, r.next = r.next[0], r.next[1:]
	r.drawn[r.last] = true

	return nil
}

func (r *RandomNumbers) Drawn(n int) bool {
	_, ok := r.drawn[n]
	return ok
}

func (r *RandomNumbers) WinningRow(row [5]int) bool {
	for _, number := range row {
		if !r.Drawn(number) {
			return false
		}
	}

	return true
}

func (r *RandomNumbers) WinningBoard(b Board) bool {
	for _, row := range b {
		if r.WinningRow(row) {
			return true
		}
	}

	// We can reuse our WinningRow method by first
	// transposing our board. If we turn every column
	// into a row, then we can trivially iterate over
	// the columns of the board as if they were rows.
	for _, column := range b.Transpose() {
		if r.WinningRow(column) {
			return true
		}
	}

	return false
}

func (r *RandomNumbers) Score(b Board) int {
	var sum int
	for _, row := range b {
		for _, number := range row {
			if !r.Drawn(number) {
				sum += number
			}
		}
	}

	return sum * r.last
}

type Board [5][5]int

func (b Board) Transpose() Board {
	var trans [5][5]int

	for i := 0; i < 5; i++ {
		for j := 0; j < 5; j++ {
			trans[j][i] = b[i][j]
		}
	}

	return trans
}

func WinFirst(r *RandomNumbers, boards []Board) (Board, error) {
	var empty [5][5]int
	for {
		if err := r.Draw(); err != nil {
			return empty, err
		}

		for _, board := range boards {
			if r.WinningBoard(board) {
				return board, nil
			}
		}
	}
}

func WinLast(r *RandomNumbers, boards []Board) (Board, error) {
	var empty [5][5]int
	for len(boards) > 1 {
		if err := r.Draw(); err != nil {
			return empty, err
		}

		candidates := make([]Board, 0, len(boards))
		for _, board := range boards {
			if !r.WinningBoard(board) {
				candidates = append(candidates, board)
			}
		}

		boards = candidates
	}

	// Even though at this point we've identified the final winning
	// board, we still need to play out the game of bingo until
	// it actually becomes a winning board.
	last := boards[0]
	for !r.WinningBoard(last) {
		if err := r.Draw(); err != nil {
			return empty, err

		}

	}

	return last, nil
}

func ParseBoard(lines []string) (Board, error) {
	var board [5][5]int
	if len(lines) != 5 {
		return board, errors.New("board must be 5 rows")
	}

	for i := 0; i < 5; i++ {
		line := strings.FieldsFunc(lines[i], func(r rune) bool {
			return r == ' '
		})

		if len(line) != 5 {
			return board, errors.New("row must be 5 columns")
		}

		for j, str := range line {
			num, err := strconv.Atoi(str)
			if err != nil {
				return board, err
			}

			board[i][j] = num
		}
	}

	return board, nil
}

func Parse(r io.Reader) (*RandomNumbers, []Board, error) {
	scanner := bufio.NewScanner(r)

	var lines []string
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}

		lines = append(lines, line)
	}

	if err := scanner.Err(); err != nil {
		return nil, nil, err
	}

	// Unlike in other problems where each line can be parsed
	// by the same function. Day 4's input has a different data structure
	// encoded on the first line and all subsequent lines.
	// Rather than parsing each line inside of the scanner.Scan loop
	// we pre-load all of the lines into a slice of strings, this way
	// we can neatly pop the first line off of the line slice, leaving us
	// with semantically unique portions of the file that need to be parsed.
	// We have the raw random number stream encoded in the first line
	// and we have a set of board encodings that should have a neat 5 lines
	// per board.
	//
	// A common alternative you might see is something that looks like:
	//
	// var rand *RandomNumbers
	// for scanner.Scan() {
	// 	if rand == nil {
	// 		var err error
	// 		rand, err = ParseRandomNumbers(scanner.Text())
	// 		if err != nil {
	// 			return nil, nil, err
	// 		}

	// 		continue
	// 	}

	// 	lines = append(lines, scanner.Text())
	// }
	//
	// Looking at the two options, I find the implementation here, the pre-loading
	// of the slice strings and then the separation of parsing concerns to be
	// the better option.
	rawNumbers, rawBoardLines := lines[0], lines[1:]
	rand, err := ParseRandomNumbers(rawNumbers)
	if err != nil {
		return nil, nil, err
	}

	if len(rawBoardLines)%5 != 0 {
		return nil, nil, errors.New("boards are expected to be exactly 5 lines long")
	}

	var boards []Board
	for len(rawBoardLines) > 0 {
		var rawBoard []string
		rawBoard, rawBoardLines = rawBoardLines[:5], rawBoardLines[5:]

		board, err := ParseBoard(rawBoard)
		if err != nil {
			return nil, nil, err
		}

		boards = append(boards, board)
	}

	return rand, boards, nil
}
