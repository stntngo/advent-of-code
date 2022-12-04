open Advent

module Solution : sig
  val part_one : string lazy_t

  val part_two : string lazy_t
end = struct
  module H = Binary_heap.Make (struct
    type t = int

    let compare = Base.Int.descending
  end)

  let stream_top_n n s =
    let heap =
      s
      |> Seq.fold_left
           (fun h s ->
             H.add h s;
             h)
           (H.create ~dummy:0 0)
    in
    Seq.forever (fun () -> H.pop_minimum heap) |> Seq.take n |> List.of_seq

  let stream =
    slurp "input/day01"
    |> Str.split (Str.regexp "\n\n")
    |> List.to_seq
    |> Seq.map (Str.split (Str.regexp "\n"))
    |> Seq.map (List.map int_of_string)
    |> Seq.map sum

  let part_one = lazy (stream |> stream_top_n 1 |> sum |> string_of_int)

  let part_two = lazy (stream |> stream_top_n 3 |> sum |> string_of_int)
end
