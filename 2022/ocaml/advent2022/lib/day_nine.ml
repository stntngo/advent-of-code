open Angstrom
open Advent

module Command = struct
  type direction =
    | Left
    | Right
    | Up
    | Down

  type t = direction * int

  let dir =
    satisfy (function
        | 'U' | 'D' | 'L' | 'R' -> true
        | _ -> false)
    >>| function
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | _ -> invalid_arg "unknown direction"

  let size =
    take_while1 (function
        | '0' .. '9' -> true
        | _ -> false)
    >>| int_of_string

  let parse = lift2 (fun dir size -> (dir, size)) dir (char ' ' *> size)
end

module Knot = struct
  type t =
    { x : int
    ; y : int
    }

  let origin = { x = 0; y = 0 }

  let move_head (d : Command.direction) k =
    match d with
    | Left -> { x = k.x - 1; y = k.y }
    | Right -> { x = k.x + 1; y = k.y }
    | Up -> { x = k.x; y = k.y + 1 }
    | Down -> { x = k.x; y = k.y - 1 }

  let follow head tail =
    match (abs (head.x - tail.x), abs (head.y - tail.y)) with
    | 2, 2 -> { x = (head.x + tail.x) / 2; y = (head.y + tail.y) / 2 }
    | 2, 1 -> { x = (head.x + tail.x) / 2; y = head.y }
    | 2, 0 -> { x = (head.x + tail.x) / 2; y = tail.y }
    | 1, 2 -> { x = head.x; y = (head.y + tail.y) / 2 }
    | 0, 2 -> { x = tail.x; y = (head.y + tail.y) / 2 }
    | _ -> tail
end

module Rope = struct
  let move1' (d : Command.direction) (rope : Knot.t Array.t) =
    Array.set rope 0 (Knot.move_head d (Array.get rope 0));
    for i = 1 to Array.length rope - 1 do
      let n = Array.get rope (i - 1) in
      let k = Array.get rope i in
      Array.set rope i (Knot.follow n k)
    done;
    rope
end

module Solution = struct
  module P = Parser.Make (Parser.Line (Command))

  module S = Set.Make (struct
    type t = Knot.t

    let compare = Stdlib.compare
  end)

  let input = P.parse "input/day09"

  let part_one =
    lazy
      (let rope = Array.init 2 (fun _ -> Knot.origin) in
       let last = Array.length rope - 1 in
       input
       |> List.map (fun (d, i) -> List.init i (fun _ -> d))
       |> List.flatten |> List.to_seq
       |> Seq.scan
            (fun _ c ->
              let _ = Rope.move1' c rope in
              Array.get rope last)
            Knot.origin
       |> S.of_seq |> S.cardinal |> string_of_int)

  let part_two =
    lazy
      (let rope = Array.init 10 (fun _ -> Knot.origin) in
       let last = Array.length rope - 1 in
       input
       |> List.map (fun (d, i) -> List.init i (fun _ -> d))
       |> List.flatten |> List.to_seq
       |> Seq.scan
            (fun _ c ->
              let _ = Rope.move1' c rope in
              Array.get rope last)
            Knot.origin
       |> S.of_seq |> S.cardinal |> string_of_int)
end
