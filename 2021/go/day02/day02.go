package day02

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"strconv"
	"strings"
)

type Direction uint

const (
	// Because our Enum value is really just a uint,
	// we want to use something like iota + 1
	// or set the first iota value to an "Invalid"
	// enum to avoid the issue in which a zero-valued
	// instance of our Enum is misinterpreted as having
	// some meaning when it's actually indicative of
	// some error state.
	//
	// This way if we accidentally write
	//
	// var direction Direction
	//
	// somewhere it won't be misinterpreted as being
	// a valid Forward direction.
	Forward Direction = iota + 1 // == 1
	Down                         // == 2
	Up                           // == 3
)

type Command struct {
	Dir      Direction
	Distance int
}

func ParseCommand(str string) (Command, error) {
	parts := strings.Split(str, " ")
	if len(parts) != 2 {
		return Command{}, errors.New("command format '[direction] [distance]'")
	}

	var dir Direction

	switch parts[0] {
	case "forward":
		dir = Forward
	case "down":
		dir = Down
	case "up":
		dir = Up
	default:
		return Command{}, errors.New("unknown direction")
	}

	i, err := strconv.Atoi(parts[1])
	if err != nil {
		// The %w formatting option allows us to wrap errors. This
		// lets us add on extra information to the error while still
		// letting upstream callers who are potentially looking to
		// react to specific kinds of errors through the use of
		// errors.Is and errors.As act on those sentinel error types.
		//
		// NOTE: If you're ever reaching to log + return an error
		// in your code, you probably want to wrap it instead!
		// That way you can add the extra informational baggage
		// you wanted to add through the logging call to the error
		// without polluting the logs so that a single error is reported
		// at more than one log site, making debugging more difficult.
		return Command{}, fmt.Errorf("can't parse distance: %w", err)
	}

	return Command{
		Dir:      dir,
		Distance: i,
	}, nil
}

type Position struct {
	x   int
	y   int
	aim int
}

func (p *Position) Move(c Command) {
	switch c.Dir {
	case Forward:
		p.x += c.Distance
	case Down:
		p.y += c.Distance
	case Up:
		p.y -= c.Distance
	}
}

func (p *Position) Aim(c Command) {
	switch c.Dir {
	case Forward:
		p.x += c.Distance
		p.y += p.aim * c.Distance
	case Down:
		p.aim += c.Distance
	case Up:
		p.aim -= c.Distance
	}
}

func (p Position) Vector() int {
	return p.x * p.y
}

func Vector(commands []Command) int {
	var p Position

	for _, command := range commands {
		p.Move(command)
	}

	return p.Vector()
}

func AimVector(commands []Command) int {
	var p Position

	for _, command := range commands {
		p.Aim(command)
	}

	return p.Vector()
}

func ParseCommands(r io.Reader) ([]Command, error) {
	var commands []Command

	scanner := bufio.NewScanner(r)

	for scanner.Scan() {
		command, err := ParseCommand(scanner.Text())
		if err != nil {
			return nil, err
		}

		commands = append(commands, command)
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return commands, nil
}
