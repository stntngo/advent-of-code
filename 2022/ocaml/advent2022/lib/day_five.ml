open Angstrom

module Command : sig
  type t =
    { amount : int
    ; source : int
    ; dest : int
    }

  val show : t -> string

  val parse : t Angstrom.t

  val apply :
    (char list -> char list -> char list) -> char list array -> t -> unit
end = struct
  type t =
    { amount : int
    ; source : int
    ; dest : int
    }
  [@@deriving show]

  let parse =
    let integer =
      take_while1 (function
          | '0' .. '9' -> true
          | _ -> false)
      >>| int_of_string
    in
    let skip_ws p = skip_many (char ' ') *> p <* skip_many (char ' ') in
    let amount = skip_ws (string "move") *> integer in
    let source = skip_ws (string "from") *> integer >>| ( + ) (-1) in
    let dest = skip_ws (string "to") *> integer >>| ( + ) (-1) in
    lift3 (fun a s d -> { amount = a; source = s; dest = d }) amount source dest

  let apply app a c =
    let stack = Array.get a c.source in
    let moved = Advent.take c.amount stack in
    let source' = Advent.drop c.amount stack in
    let dest = Array.get a c.dest in
    let dest' = app moved dest in
    Array.set a c.source source';
    Array.set a c.dest dest'
end

module Parser : sig
  type t = char list array * Command.t list

  val parse : t Angstrom.t
end = struct
  module Commands = Advent.Parser.Line (Command)

  type t = char list array * Command.t list

  let parse_stacks =
    let line =
      let payload =
        let non_empty = char '[' *> any_char <* char ']' in
        let empty = char ' ' *> char ' ' <* char ' ' in
        non_empty <|> empty
      in
      sep_by (char ' ') payload >>| fun l -> l |> List.mapi (fun i v -> (i, v))
    in
    sep_by (string "\n") line >>| fun l ->
    let len = l |> List.hd |> List.length in
    let stacks = Array.make len [] in
    l |> List.flatten |> List.rev |> List.to_seq
    |> Seq.filter (fun (_, v) -> v != ' ')
    |> Seq.iter (fun (i, v) ->
           let existing = Array.get stacks i in
           Array.set stacks i (v :: existing));
    stacks

  let skip_axis =
    let label =
      satisfy (function
          | '0' .. '9' -> true
          | _ -> false)
    in
    let skip_ws p = skip_many (char ' ') *> p <* skip_many (char ' ') in
    skip_many1 (skip_ws label) <* skip_many (string "\n")

  let parse = lift3 (fun s _ c -> (s, c)) parse_stacks skip_axis Commands.parse
end

module Solution : sig
  val part_one : string lazy_t

  val part_two : string lazy_t
end = struct
  module P = Advent.Parser.Make (Parser)

  let stacks, commands = P.parse "input/day05"

  let part_one =
    lazy
      (let stacks = Array.copy stacks in
       commands |> List.iter (Command.apply List.rev_append stacks);
       stacks |> Array.map List.hd |> Array.to_seq |> String.of_seq)

  let part_two =
    lazy
      (let stacks = Array.copy stacks in
       commands |> List.iter (Command.apply List.append stacks);
       stacks |> Array.map List.hd |> Array.to_seq |> String.of_seq)
end
