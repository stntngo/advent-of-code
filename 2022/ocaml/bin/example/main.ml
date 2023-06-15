open Advent2022.Example
open Dist

let tuple x y = (x, y)

let pp_print_tuple f state (x, y) =
  let s = "(" ^ f x ^ ", " ^ f y ^ ")" in
  Format.pp_print_as state (String.length s) s

let () =
  tuple <$> die 5 <*> die 4
  |> Dist.show (pp_print_tuple string_of_int)
  |> print_endline
