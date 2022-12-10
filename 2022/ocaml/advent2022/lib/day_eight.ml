open Angstrom

module Forest = struct
  type t = int Seq.t Seq.t

  let parse =
    sep_by1
      (string "\n")
      (many1
         (satisfy (function
              | '0' .. '9' -> true
              | _ -> false)
         >>| String.make 1 >>| int_of_string)
      >>| List.to_seq)
    >>| List.to_seq

  let visible' (l : int Seq.t) =
    match l () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (hd, tl) ->
      Seq.scan (fun (_, el) x -> (x > el, max x el)) (true, hd) tl
      |> Seq.map fst

  let visible l =
    let forward = visible' l in
    let backward = l |> Advent.rev |> visible' |> Advent.rev in
    Seq.zip forward backward |> Seq.map (fun (l, r) -> l || r)

  let tree_cover forest =
    let row_wise = forest |> Seq.map visible |> Seq.concat in
    let col_wise =
      forest |> Seq.transpose |> Seq.map visible |> Seq.transpose |> Seq.concat
    in
    Seq.map2 (fun x y -> x || y) row_wise col_wise
    |> Seq.filter Fun.id |> Seq.length

  let sight_lines x y a =
    let lines = Array.make 4 [] in
    (* north *)
    for i = 0 to y - 1 do
      let line = Array.get lines 0 in
      Array.set lines 0 (Array.get (Array.get a i) x :: line)
    done;

    (* south *)
    for i = y + 1 to Array.length a - 1 do
      let line = Array.get lines 1 in
      Array.set lines 1 (Array.get (Array.get a i) x :: line)
    done;
    Array.set lines 1 (List.rev (Array.get lines 1));

    let width = Array.length (Array.get a 0) in

    (* west *)
    for i = 0 to x - 1 do
      let line = Array.get lines 2 in
      Array.set lines 2 (Array.get (Array.get a y) i :: line)
    done;

    (* east *)
    for i = x + 1 to width - 1 do
      let line = Array.get lines 3 in
      Array.set lines 3 (Array.get (Array.get a y) i :: line)
    done;
    Array.set lines 3 (List.rev (Array.get lines 3));

    lines

  let score' h l =
    List.mapi (fun i t -> (i, t)) l |> List.find_opt (fun (_, t) -> t >= h)
    |> function
    | None -> List.length l
    | Some (i, _) -> i + 1

  let scenic_score x y a =
    let height = Array.get (Array.get a y) x in
    let lines = sight_lines x y a in
    Array.map (score' height) lines |> Array.fold_left (fun x y -> x * y) 1
end

module Solution = struct
  module P = Advent.Parser.Make (Forest)

  let forest = P.parse "input/day08"

  let forest_array = forest |> Seq.map Array.of_seq |> Array.of_seq

  let part_one = lazy "omitted"

  let _ = lazy (forest |> Forest.tree_cover |> string_of_int)

  let part_two = lazy "omitted"

  let _ =
    lazy
      (forest_array
      |> Array.mapi (fun y a ->
             Array.mapi (fun x _ -> Forest.scenic_score x y forest_array) a)
      |> Array.to_list |> Array.concat |> Array.fold_left max (-1)
      |> string_of_int)
end
