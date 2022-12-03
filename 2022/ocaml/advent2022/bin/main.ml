module Day_one = Advent2022.Day_one
module Day_two = Advent2022.Day_two
module Day_three = Advent2022.Day_three

let days =
  [ (Day_one.part_one, Day_one.part_two)
  ; (Day_two.part_one, Day_two.part_two)
  ; (Day_three.part_one, Day_three.part_two)
  ]

let print_day day =
  let part_one, part_two = day in
  print_endline ("=> Part One: " ^ Lazy.force part_one);
  print_endline ("=> Part Two: " ^ Lazy.force part_two)

let print_results days =
  let i = 1 in
  let rec printrec days i =
    match days with
    | [] -> ()
    | day :: days ->
      print_endline ("Day " ^ string_of_int i);
      print_day day;
      printrec days (i + 1)
  in
  printrec days i

let () = print_results days
