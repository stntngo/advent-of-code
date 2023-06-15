open Angstrom
open Advent

module Instruction = struct
  type t =
    | Noop
    | Addx of int
  [@@deriving show]

  let integer1 =
    char '-'
    <|> satisfy (function
            | '0' .. '9' -> true
            | _ -> false)

  let integer =
    take_while (function
        | '0' .. '9' -> true
        | _ -> false)
    |> both integer1 |> consumed >>| int_of_string

  let parse_noop = string "noop" >>| fun _ -> Noop

  let parse_addx = string "addx" *> char ' ' *> integer >>| fun i -> Addx i

  let parse = parse_noop <|> parse_addx
end

module CPU = struct
  type t =
    { instructions : Instruction.t array
    ; mutable reg_x : int
    ; mutable counter : int
    }
  [@@deriving show]

  let init instructions = { instructions; reg_x = 1; counter = 1 }

  let run interesting cpu =
    let signals = ref [] in
    Array.iter
      (fun i ->
        let width =
          match i with
          | Instruction.Noop -> 1
          | Instruction.Addx _ -> 2
        in
        for _ = 1 to width do
          if
            List.find_opt (fun x -> x == cpu.counter) interesting
            |> Option.is_some
          then signals := (cpu.counter * cpu.reg_x) :: !signals;

          cpu.counter <- cpu.counter + 1
        done;

        match i with
        | Instruction.Addx amt -> cpu.reg_x <- cpu.reg_x + amt
        | Instruction.Noop -> ())
      cpu.instructions;
    !signals

  let draw w cpu =
    let scan = ref [] in
    Array.iter
      (fun i ->
        let width =
          match i with
          | Instruction.Noop -> 1
          | Instruction.Addx _ -> 2
        in
        for _ = 1 to width do
          let px = (cpu.counter - 1) mod w in
          let glyph = if abs (px - cpu.reg_x) <= 1 then '#' else '.' in
          scan := glyph :: !scan;

          cpu.counter <- cpu.counter + 1
        done;

        match i with
        | Instruction.Addx amt -> cpu.reg_x <- cpu.reg_x + amt
        | Instruction.Noop -> ())
      cpu.instructions;
    !scan
end

module Solution = struct
  module P = Parser.Make (Parser.Line (Instruction))

  let input = P.parse "input/day10" |> Array.of_list

  let part_one =
    lazy
      (input |> CPU.init
      |> CPU.run [ 20; 60; 100; 140; 180; 220 ]
      |> sum |> string_of_int)

  let part_two =
    let w = 40 in
    lazy
      ("\n"
      ^ (input |> CPU.init |> CPU.draw w |> List.rev |> List.chunk w
       |> List.map List.to_seq |> List.map String.of_seq |> String.concat "\n")
      )
end
