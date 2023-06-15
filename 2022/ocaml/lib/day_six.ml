module Solution : sig
  val part_one : string lazy_t

  val part_two : string lazy_t
end = struct
  module S = Set.Make (struct
    type t = char

    let compare = Stdlib.compare
  end)

  let packeti n p =
    p |> String.to_seq |> Advent.window n |> Seq.map S.of_seq
    |> Seq.map S.cardinal
    |> Seq.take_while (( > ) n)
    |> Seq.length |> ( + ) n

  let part_one = lazy (Advent.slurp "input/day06" |> packeti 4 |> string_of_int)

  let part_two = lazy (Advent.slurp "input/day06" |> packeti 14 |> string_of_int)
end
