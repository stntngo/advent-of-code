module Solution : sig
  val part_one : string lazy_t

  val part_two : string lazy_t
end = struct
  module S = Set.Make (struct
    type t = char

    let compare = Stdlib.compare
  end)

  let bisect_string s =
    let mid = String.length s / 2 in
    Advent.chunk mid (String.to_seq s) |> Seq.map S.of_seq

  let common_item (rucksacks : S.t Seq.t) =
    match rucksacks () with
    | Seq.Nil -> failwith "common_item"
    | Seq.Cons (x, xs) -> Seq.fold_left S.inter x xs |> S.min_elt

  let priority c =
    let start_lower = Char.code 'a' - 1
    and start_upper = Char.code 'A' - 1
    and p = Char.code c in
    if p > start_lower then p - start_lower else p - start_upper + 26

  let input = Advent.slurp "input/day03" |> Advent.lines

  let part_one =
    lazy
      (input |> Seq.map bisect_string |> Seq.map common_item |> Seq.map priority
     |> List.of_seq |> Advent.sum |> string_of_int)

  let part_two =
    lazy
      (input |> Seq.map String.to_seq |> Seq.map S.of_seq |> Advent.chunk 3
     |> Seq.map common_item |> Seq.map priority |> List.of_seq |> Advent.sum
     |> string_of_int)
end

module Polynomial = struct
  type t = float list

  let evaluate (p : t) (x : float) : float =
    List.fold_left
      ( +. )
      0.0
      (List.mapi (fun i a -> a *. (x ** float_of_int i)) p)
end
