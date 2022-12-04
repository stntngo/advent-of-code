module Day_one = Advent2022.Day_one
module Day_two = Advent2022.Day_two
module Day_three = Advent2022.Day_three
module Day_four = Advent2022.Day_four

let days =
  [ (Day_one.Solution.part_one, Day_one.Solution.part_two)
  ; (Day_two.Solution.part_one, Day_two.Solution.part_two)
  ; (Day_three.Solution.part_one, Day_three.Solution.part_two)
  ; (Day_four.Solution.part_one, Day_four.Solution.part_two)
  ]

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "=> (%f ms)\n\n" ((Unix.gettimeofday () -. t) *. 1000.);
  res

let print_day day () =
  let part_one, part_two = day in
  print_endline ("=> Part One: " ^ Lazy.force part_one);
  print_endline ("=> Part Two: " ^ Lazy.force part_two)

let print_results days () =
  let i = 1 in
  let rec printrec days i =
    match days with
    | [] -> ()
    | day :: days ->
      print_endline ("Day " ^ string_of_int i);
      time (print_day day);
      printrec days (i + 1)
  in
  printrec days i

let () = time (print_results days)
