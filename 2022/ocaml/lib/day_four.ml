open Angstrom
open Advent

module Shift : sig
  type t

  val is_contained : t -> bool
  val is_intersecting : t -> bool
  val parse : t Angstrom.t
end = struct
  type t' = { lo : int; hi : int }
  type t = t' * t'

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let is_between lo hi x = lo <= x && x <= hi

  let is_contained = function
    | { lo = l1; hi = l2 }, { lo = r1; hi = r2 } ->
        (is_between l1 l2 r1 && is_between l1 l2 r2)
        || (is_between r1 r2 l1 && is_between r1 r2 l2)

  let is_intersecting = function
    | { lo = l1; hi = l2 }, { lo = r1; hi = r2 } ->
        is_between r1 r2 l1 || is_between r1 r2 l2 || is_between l1 l2 r1
        || is_between l1 l2 r2

  let parse' = lift3 (fun lo _ hi -> { lo; hi }) integer (char '-') integer
  let parse = lift3 (fun x _ y -> (x, y)) parse' (char ',') parse'
end

module Solution : sig
  val part_one : string lazy_t
  val part_two : string lazy_t
end = struct
  module P = Parser.Make (Parser.Line (Shift))

  let assignments = lazy (P.parse "input/day04" |> List.to_seq)

  let part_one =
    lazy
      (assignments |> Lazy.force
      |> Seq.filter Shift.is_contained
      |> Seq.length |> string_of_int)

  let part_two =
    lazy
      (assignments |> Lazy.force
      |> Seq.filter Shift.is_intersecting
      |> Seq.length |> string_of_int)
end
