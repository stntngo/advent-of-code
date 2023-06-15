open Advent
open Angstrom

module Choice = struct
  type t =
    | Rock
    | Paper
    | Scissors

  let better = function
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock

  let worse = function
    | Rock -> Scissors
    | Paper -> Rock
    | Scissors -> Paper

  let points = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

  let score_game game =
    match game with
    | a, b when a == b -> 3 + points b
    | a, b when b == better a -> 6 + points b
    | _, b -> 0 + points b

  let of_char base c =
    match Char.code c - Char.code base with
    | 0 -> Rock
    | 1 -> Paper
    | 2 -> Scissors
    | _ -> failwith "parse choice"

  let bind = ( >>= )

  let or' = ( <|> )

  let chainl1 e op =
    let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
    e >>= fun init -> go init

  let chainl2 e op =
    let rec go acc =
      or' (lift2 (fun f x -> f acc x) op (bind e go)) (return acc)
    in
    bind e (fun init -> go init)

  let parse base = any_char >>| of_char base
end

module Strategy = struct
  type t =
    | Win
    | Lose
    | Tie

  let to_game = function
    | opp, Win -> (opp, Choice.better opp)
    | opp, Lose -> (opp, Choice.worse opp)
    | opp, Tie -> (opp, opp)

  let parse =
    any_char >>| function
    | 'X' -> Lose
    | 'Y' -> Tie
    | 'Z' -> Win
    | _ -> failwith "parse strategy"
end

module Solution : sig
  val part_one : string lazy_t

  val part_two : string lazy_t
end = struct
  let round ours =
    lift3 (fun x _ y -> (x, y)) (Choice.parse 'A') (char ' ') ours

  let input = slurp "input/day02" |> lines

  let part_one =
    lazy
      (input
      |> Seq.map (parse (round (Choice.parse 'X')))
      |> Seq.map Choice.score_game |> List.of_seq |> sum |> string_of_int)

  let part_two =
    lazy
      (input
      |> Seq.map (parse (round Strategy.parse))
      |> Seq.map Strategy.to_game |> Seq.map Choice.score_game |> List.of_seq
      |> sum |> string_of_int)
end
