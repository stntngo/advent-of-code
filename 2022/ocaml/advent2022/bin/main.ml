module Day_one = Advent2022.Day_one

let days =
  [ ( lazy (Lazy.force Day_one.input |> Day_one.top_n_sum 1 |> string_of_int)
    , lazy (Lazy.force Day_one.input |> Day_one.top_n_sum 3 |> string_of_int) )
  ]

let print_day day =
  let part_one, part_two = day in
  print_endline ("=> Part One: " ^ Lazy.force part_one);
  print_endline ("=> Part Two: " ^ Lazy.force part_two)

let rec print_results ?(i = 1) days =
  match days with
  | [] -> ()
  | day :: days ->
    print_endline ("Day " ^ string_of_int i);
    print_day day;
    print_results days ~i:(i + 1)

let () = print_results days
