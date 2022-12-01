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
