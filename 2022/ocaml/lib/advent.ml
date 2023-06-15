open Angstrom

let slurp p = In_channel.with_open_text p In_channel.input_all

let const a _ = a

module Array = struct
  include Array

  let get_opt i a =
    match i with
    | i when i < 0 || i >= Array.length a -> None
    | _ -> Some a.(i)
end

module List = struct
  include List

  let rec sum l =
    match l with
    | [] -> 0
    | h :: t -> h + sum t

  let rec take k xs =
    match k with
    | 0 -> []
    | k -> (
      match xs with
      | [] -> failwith "take"
      | y :: ys -> y :: take (k - 1) ys)

  let rec drop k xs =
    match k with
    | 0 -> xs
    | k -> (
      match xs with
      | [] -> []
      | _ :: ys -> drop (k - 1) ys)

  let rec chunk n xs =
    match xs with
    | [] -> []
    | _ -> take n xs :: chunk n (drop n xs)
end

let rec sum l =
  match l with
  | [] -> 0
  | h :: t -> h + sum t

let rec take k xs =
  match k with
  | 0 -> []
  | k -> (
    match xs with
    | [] -> failwith "take"
    | y :: ys -> y :: take (k - 1) ys)

let rec drop k xs =
  match k with
  | 0 -> xs
  | k -> (
    match xs with
    | [] -> failwith "drop"
    | _ :: ys -> drop (k - 1) ys)

module Seq = struct
  include Seq

  let chunk n xs =
    let rec chunk_aux xs () =
      match xs () with
      | Seq.Nil -> Seq.Nil
      | _ -> Seq.Cons (Seq.take n xs, chunk_aux (Seq.drop n xs))
    in
    chunk_aux xs

  let window n xs =
    let rec window_aux xs () =
      match xs () with
      | Seq.Nil -> Seq.Nil
      | _ -> Seq.Cons (Seq.take n xs, window_aux (Seq.drop 1 xs))
    in
    window_aux xs

  let rev xs =
    let rec rev_aux t xs =
      match xs () with
      | Seq.Nil -> t
      | Seq.Cons (x, xs) -> rev_aux (Seq.cons x t) xs
    in
    rev_aux Seq.empty xs
end

let chunk n xs =
  let rec chunk_aux xs () =
    match xs () with
    | Seq.Nil -> Seq.Nil
    | _ -> Seq.Cons (Seq.take n xs, chunk_aux (Seq.drop n xs))
  in
  chunk_aux xs

let window n xs =
  let rec window_aux xs () =
    match xs () with
    | Seq.Nil -> Seq.Nil
    | _ -> Seq.Cons (Seq.take n xs, window_aux (Seq.drop 1 xs))
  in
  window_aux xs

let rev xs =
  let rec rev_aux t xs =
    match xs () with
    | Seq.Nil -> t
    | Seq.Cons (x, xs) -> rev_aux (Seq.cons x t) xs
  in
  rev_aux Seq.empty xs

let lines s = s |> Str.split (Str.regexp "\n") |> List.to_seq

module Parser = struct
  module type P = sig
    type t

    val parse : t Angstrom.t
  end

  module Line (P : P) = struct
    type t = P.t list

    let parse = sep_by (string "\n") P.parse
  end

  module Make (P : P) = struct
    let parse s =
      let body = slurp s in
      match parse_string ~consume:Prefix P.parse body with
      | Ok r -> r
      | Error msg -> failwith msg
  end
end

let parse p s =
  match parse_string ~consume:Prefix p s with
  | Ok r -> r
  | Error msg -> failwith msg

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "=> (%f ms)\n\n" ((Unix.gettimeofday () -. t) *. 1000.);
  res

let print_day day () =
  let part_one, part_two = day in
  print_endline ("=> Part One: " ^ Lazy.force part_one);
  print_endline ("=> Part Two: " ^ Lazy.force part_two)

let print_results days () =
  let i = 1 in
  let rec printrec days i =
    match days with
    | [] -> ()
    | day :: days ->
      print_endline ("Day " ^ string_of_int i);
      time (print_day day);
      printrec days (i + 1)
  in
  printrec days i
