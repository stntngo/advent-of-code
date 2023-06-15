module Day_one = Advent2022.Day_one
module Day_two = Advent2022.Day_two
module Day_three = Advent2022.Day_three
module Day_four = Advent2022.Day_four
module Day_five = Advent2022.Day_five
module Day_six = Advent2022.Day_six
module Day_seven = Advent2022.Day_seven
module Day_eight = Advent2022.Day_eight
module Day_nine = Advent2022.Day_nine
module Day_ten = Advent2022.Day_ten
module Day_eleven = Advent2022.Day_eleven
module Day_twelve = Advent2022.Day_twelve
module Advent = Advent2022.Advent

let days =
  [ (Day_one.Solution.part_one, Day_one.Solution.part_two)
  ; (Day_two.Solution.part_one, Day_two.Solution.part_two)
  ; (Day_three.Solution.part_one, Day_three.Solution.part_two)
  ; (Day_four.Solution.part_one, Day_four.Solution.part_two)
  ; (Day_five.Solution.part_one, Day_five.Solution.part_two)
  ; (Day_six.Solution.part_one, Day_six.Solution.part_two)
  ; (Day_seven.Solution.part_one, Day_seven.Solution.part_two)
  ; (Day_eight.Solution.part_one, Day_eight.Solution.part_two)
  ; (Day_nine.Solution.part_one, Day_nine.Solution.part_two)
  ; (Day_ten.Solution.part_one, Day_ten.Solution.part_two)
  ; (Day_eleven.Solution.part_one, Day_eleven.Solution.part_two)
  ; (Day_twelve.Solution.part_one, Day_twelve.Solution.part_two)
  ]

let () = Advent.time (Advent.print_results days)
