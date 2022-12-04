open Angstrom

let slurp p = In_channel.with_open_text p In_channel.input_all

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

let chunk n xs =
  let rec chunk_aux xs () =
    match xs () with
    | Seq.Nil -> Seq.Nil
    | _ -> Seq.Cons (Seq.take n xs, chunk_aux (Seq.drop n xs))
  in
  chunk_aux xs

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
