package main

import (
	"embed"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/olekukonko/tablewriter"
	"github.com/stntngo/advent-2021/go/day01"
	"github.com/stntngo/advent-2021/go/day02"
	"github.com/stntngo/advent-2021/go/day03"
	"github.com/stntngo/advent-2021/go/day04"
	"github.com/stntngo/advent-2021/go/day05"
	"github.com/stntngo/advent-2021/go/day06"
	"github.com/stntngo/advent-2021/go/day07"
	"github.com/stntngo/advent-2021/go/day08"
	"github.com/stntngo/advent-2021/go/day09"
	"github.com/stntngo/advent-2021/go/day10"
	"github.com/stntngo/advent-2021/go/day11"
	"github.com/stntngo/advent-2021/go/day12"
	"github.com/stntngo/advent-2021/go/day13"
	"github.com/stntngo/advent-2021/go/day14"
	"github.com/stntngo/advent-2021/go/day15"
	"github.com/stntngo/advent-2021/go/day16"
)

type Solution interface {
	Name() string
	Load(io.Reader) error
	PartOne() (string, error)
	PartTwo() (string, error)
}

//go:embed input
var inputs embed.FS

var solutions = []Solution{
	new(day01.Solution),
	new(day02.Solution),
	new(day03.Solution),
	new(day04.Solution),
	new(day05.Solution),
	new(day06.Solution),
	new(day07.Solution),
	new(day08.Solution),
	new(day09.Solution),
	new(day10.Solution),
	new(day11.Solution),
	new(day12.Solution),
	new(day13.Solution),
	new(day14.Solution),
	new(day15.Solution),
	new(day16.Solution),
}

func main() {
	table := make([][]string, 0, len(solutions))
	ttstart := time.Now()
	for i, sol := range solutions {
		func() {
			tstart := time.Now()
			f, err := inputs.Open(fmt.Sprintf("input/day-%02d", i+1))
			if err != nil {
				panic(err)
			}
			defer f.Close()

			if err := sol.Load(f); err != nil {
				panic(err)
			}

			pone, err := sol.PartOne()
			if err != nil {
				panic(err)
			}

			ptwo, err := sol.PartTwo()
			if err != nil {
				panic(err)
			}
			tend := time.Now()

			row := []string{
				fmt.Sprintf("Day %v", i+1),
				sol.Name(),
				pone,
				ptwo,
				fmt.Sprintf("%s", tend.Sub(tstart)),
			}

			table = append(table, row)
		}()
	}
	ttend := time.Now()

	w := tablewriter.NewWriter(os.Stdout)
	w.SetHeader([]string{"Day", "Name", "Part One", "Part Two", "Duration"})
	w.AppendBulk(table)
	w.Append([]string{"All Days", "", "", "", fmt.Sprintf("%s", ttend.Sub(ttstart))})

	fmt.Println("Advent of Code 2021!")
	w.SetHeaderAlignment(tablewriter.ALIGN_RIGHT)
	w.SetAlignment(tablewriter.ALIGN_RIGHT)
	w.Render()
}
