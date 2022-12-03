open Advent

module C = struct
  type t = char

  let compare = Stdlib.compare
end

module S = Set.Make (C)

let bisect_string s =
  let mid = String.length s / 2 in
  chunk mid (String.to_seq s) |> Seq.map S.of_seq

let common_item (rucksacks : S.t Seq.t) =
  match rucksacks () with
  | Seq.Nil -> failwith "common_item"
  | Seq.Cons (x, xs) -> Seq.fold_left S.inter x xs |> S.min_elt

let priority c =
  let start_lower = Char.code 'a' - 1
  and start_upper = Char.code 'A' - 1
  and p = Char.code c in
  if p > start_lower then p - start_lower else p - start_upper + 26

let input = slurp "input/day03" |> Str.split (Str.regexp "\n")

let part_one =
  lazy
    (input |> List.to_seq |> Seq.map bisect_string |> Seq.map common_item
   |> Seq.map priority |> List.of_seq |> sum |> string_of_int)

let part_two =
  lazy
    (input |> List.to_seq |> Seq.map String.to_seq |> Seq.map S.of_seq
   |> chunk 3 |> Seq.map common_item |> Seq.map priority |> List.of_seq |> sum
   |> string_of_int)
