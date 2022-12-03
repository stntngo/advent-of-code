open Advent

type choice =
  | Rock
  | Paper
  | Scissors

let loses c =
  match c with
  | Rock -> Paper
  | Scissors -> Rock
  | Paper -> Scissors

let beats c =
  match c with
  | Rock -> Scissors
  | Scissors -> Paper
  | Paper -> Rock

let parse_move base x =
  match Char.code x.[0] - Char.code base with
  | 0 -> Rock
  | 1 -> Paper
  | 2 -> Scissors
  | _ -> failwith "parse_move"

let decode_move x y =
  let opponent = parse_move 'A' x in
  match y with
  | "X" -> beats opponent
  | "Y" -> opponent
  | "Z" -> loses opponent
  | _ -> failwith "loses_to"

let parse_game (decode : string -> string -> choice) (l : string list) :
    choice * choice =
  match l with
  | x :: y :: _ -> (parse_move 'A' x, decode x y)
  | _ -> failwith "parse_game"

let score_game' choice =
  match choice with
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3

let score_game game =
  match game with
  | a, b when a == b -> 3 + score_game' b
  | a, b when b == loses a -> 6 + score_game' b
  | _, b -> 0 + score_game' b

let input =
  slurp "input/day02"
  |> Str.split (Str.regexp "\n")
  |> List.to_seq
  |> Seq.map (Str.split (Str.regexp " "))

let part_one =
  lazy
    (input
    |> Seq.map (fun l ->
           l |> parse_game (fun _ y -> parse_move 'X' y) |> score_game)
    |> Seq.fold_left (fun x y -> x + y) 0
    |> string_of_int)

let part_two =
  lazy
    (input
    |> Seq.map (fun l -> l |> parse_game decode_move |> score_game)
    |> Seq.fold_left (fun x y -> x + y) 0
    |> string_of_int)
