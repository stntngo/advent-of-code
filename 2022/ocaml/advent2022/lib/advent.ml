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
