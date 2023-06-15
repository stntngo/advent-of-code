module Prob : sig
  type t = float [@@deriving show]

  val mempty : t
end = struct
  type t = float [@@deriving show]

  let mempty = 0.0
end

module Dist : sig
  type 'a t = ('a * Prob.t) list [@@deriving show]

  val fmap : ('a -> 'b) -> 'a t -> 'b t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t

  val unpack : 'a t -> ('a * Prob.t) list

  val sump : 'a t -> Prob.t

  val normp : 'a t -> 'a t

  val squishd : 'a t -> 'a t
end = struct
  type 'a t = ('a * Prob.t) list [@@deriving show]

  let group_by (f : 'a * Prob.t -> 'b) (ll : 'a t) : 'b t =
    (List.fold_left
       (fun acc e ->
         let grp = f e in
         let grp_mem = try Hashtbl.find acc grp with Not_found -> 0.0 in
         Hashtbl.replace acc grp (snd e +. grp_mem);
         acc)
       (Hashtbl.create 100)
       ll
    |> Hashtbl.fold (fun x p d -> (x, p) :: d))
      []

  let squishd xs = group_by fst xs

  let unpack d = d

  let sump d = List.map snd d |> List.fold_left ( +. ) Prob.mempty

  let normp d =
    let q = sump d in
    List.map (fun (x, p) -> (x, p /. q)) d

  let fmap f d = List.map (fun (x, p) -> (f x, p)) d

  let ( <$> ) = fmap

  let pure x = [ (x, 1.0) ]

  let rec combine (l1 : 'a t) (l2 : 'b t) =
    match (l1, l2) with
    | [], [] -> []
    | a1 :: l1, a2 :: l2 -> (a1, a2) :: combine l1 l2
    | _, _ -> invalid_arg "combine"

  let ( <*> ) f xs =
    combine f xs |> List.map (fun ((f, pf), (a, px)) -> (f a, pf *. px))
end

module Event : sig
  type 'a t = 'a -> bool

  val evald : 'a t -> 'a Dist.t -> Prob.t
end = struct
  type 'a t = 'a -> bool

  let evald (p : 'a t) (d : 'a Dist.t) =
    List.filter (fun x -> p (fst x)) d |> Dist.sump
end

let uniform xs = List.map (fun x -> (x, 1.0)) xs |> Dist.normp

let die n = List.init n (( + ) 1) |> uniform

let coin f x y : 'a Dist.t =
  match f with
  | p when p < 0.0 || p > 1.0 -> failwith "probability must be 0 <= p <= 1"
  | f -> [ (x, f); (y, 1.0 -. f) ]
