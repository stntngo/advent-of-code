open Advent
open Streaming

module Int = struct
  type t = int

  let compare = Base.Int.descending
end

module H = Binary_heap.Make (Int)

let stream_top_n n s =
  let heap =
    s
    |> Stream.fold
         (fun h s ->
           H.add h s;
           h)
         (H.create ~dummy:0 0)
  in
  Stream.repeatedly ~times:n (fun () -> H.pop_minimum heap) |> Stream.to_list

let stream =
  slurp "input/day01"
  |> Str.split (Str.regexp "\n\n")
  |> Stream.of_list
  |> Stream.map (Str.split (Str.regexp "\n"))
  |> Stream.map (List.map int_of_string)
  |> Stream.map sum
