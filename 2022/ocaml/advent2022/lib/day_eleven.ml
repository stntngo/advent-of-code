open Angstrom
open Advent

module type Integral = sig
  type t

  val zero : t

  val one : t

  val equal : t -> t -> bool

  val rem : t -> t -> t

  val add : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val of_string : string -> t

  val to_string : t -> string

  val of_int : int -> t

  val to_int : t -> int
end

module Math (I : Integral) = struct
  include I

  let rec gcd u v = if I.equal v I.zero then u else gcd v (I.rem u v)

  let lcm m n =
    match (m, n) with
    | x, _ when I.equal x I.zero -> I.zero
    | _, x when I.equal x I.zero -> I.zero
    | m, n -> I.div (I.mul m n) (gcd m n)
end

module Monkey (I : Integral) = struct
  type t =
    { id : I.t
    ; mutable items : I.t array
    ; operation : I.t -> I.t
    ; test : I.t -> int
    ; divisor : I.t
    ; mutable inspections : int
    }

  type node =
    | Old
    | Lit of I.t

  let inspect_count m = m.inspections |> I.of_int

  let divisor m = m.divisor

  let inspect (w : I.t -> I.t) (m : t) (ms : t array) =
    let inspected = Array.map (fun i -> w (m.operation i)) m.items in
    m.inspections <- m.inspections + Array.length inspected;
    m.items <- Array.make 0 I.zero;
    Array.iter
      (fun i ->
        let dest = Array.get ms (m.test i) in
        dest.items <- Array.append dest.items [| i |])
      inspected

  let parse =
    let ws =
      skip_while (function
          | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
          | _ -> false)
    in

    let skip_ws p = ws *> p <* ws in

    let integer =
      take_while1 (function
          | '0' .. '9' -> true
          | _ -> false)
      >>| I.of_string
    in

    let monkey =
      skip_ws (string "Monkey") *> integer <* char ':' <?> "monkey"
    in

    let items =
      skip_ws (string "Starting items:" <?> "starting items")
      *> sep_by (string ", ") integer
      >>| Array.of_list <?> "items"
    in

    let operation =
      let op =
        satisfy (function
            | '+' | '-' | '/' | '*' -> true
            | _ -> false)
        >>| function
        | '+' -> I.add
        | '*' -> I.mul
        | _ -> invalid_arg "bad op"
      in

      let old = string "old" >>| const Old in

      let arg = old <|> (integer >>| fun i -> Lit i) in

      skip_ws (string "Operation: new =")
      *> lift3
           (fun x op y old ->
             let arg1 =
               match x with
               | Old -> old
               | Lit i -> i
             in

             let arg2 =
               match y with
               | Old -> old
               | Lit i -> i
             in
             op arg1 arg2)
           (skip_ws arg)
           (skip_ws op)
           (skip_ws arg)
      <?> "operation"
    in

    let predicate =
      skip_ws (string "Test: divisible by") *> integer >>| fun x ->
      (x, fun y -> I.equal (I.rem y x) I.zero)
    in

    let target s =
      skip_ws (string "If " *> string s *> string ": throw to monkey")
      *> integer
      >>| I.to_int
    in

    let test =
      lift3
        (fun (d, p) t f -> (d, fun n -> if p n then t else f))
        predicate
        (target "true")
        (target "false")
      <?> "test"
    in

    skip_ws
      (lift4
         (fun m i o (d, t) ->
           { id = m
           ; items = i
           ; operation = o
           ; test = t
           ; divisor = d
           ; inspections = 0
           })
         monkey
         items
         operation
         test)
end

module Solution = struct
  module I = Math (Int64)
  module M = Monkey (I)

  let part_one =
    lazy
      (let monkeys =
         slurp "input/day11" |> Advent.parse (many1 M.parse) |> Array.of_list
       in
       for _ = 1 to 20 do
         Array.iter
           (fun m -> M.inspect ((Fun.flip I.div) (I.of_int 3)) m monkeys)
           monkeys
       done;
       monkeys |> Array.to_list |> List.map M.inspect_count |> List.sort compare
       |> List.rev |> List.take 2 |> List.fold_left I.mul I.one |> I.to_string)

  let part_two =
    lazy
      (let monkeys =
         slurp "input/day11" |> Advent.parse (many1 M.parse) |> Array.of_list
       in
       let modulus =
         monkeys |> Array.map M.divisor |> Array.fold_left I.lcm I.one
       in
       for _ = 1 to 10000 do
         Array.iter
           (fun m -> M.inspect ((Fun.flip I.rem) modulus) m monkeys)
           monkeys
       done;
       monkeys |> Array.to_list |> List.map M.inspect_count |> List.sort compare
       |> List.rev |> List.take 2 |> List.fold_left I.mul I.one |> I.to_string)
end
