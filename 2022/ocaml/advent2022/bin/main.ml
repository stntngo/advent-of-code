module Day_one = Advent2022.Day_one
module Day_two = Advent2022.Day_two
open Advent2022.Advent
open Streaming

let days =
  [ ( lazy (Day_one.stream |> Day_one.stream_top_n 1 |> sum |> string_of_int)
    , lazy (Day_one.stream |> Day_one.stream_top_n 3 |> sum |> string_of_int) )
  ; ( lazy
        (Day_two.input
        |> Stream.map (fun l ->
               l
               |> Day_two.parse_game (fun _ y -> Day_two.parse_move 'X' y)
               |> Day_two.score_game)
        |> Stream.fold (fun x y -> x + y) 0
        |> string_of_int)
    , lazy
        (Day_two.input
        |> Stream.map (fun l ->
               l |> Day_two.parse_game Day_two.decode_move |> Day_two.score_game)
        |> Stream.fold (fun x y -> x + y) 0
        |> string_of_int) )
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
