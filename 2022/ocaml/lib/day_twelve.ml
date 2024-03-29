type node = int
type edge = node * node
type path = node list
type tree = path list
type 'a node' = node * 'a
type 'b edge' = node * node * 'b
type 'a path' = 'a node' list
type 'a tree' = 'a path' list

type ('a, 'b) context =
  | Context of ('b * node) list * node * 'a * ('b * node) list

module Graph (G : sig
  type ('a, 'b) t

  val ( & ) : ('a, 'b) context -> ('a, 'b) t -> ('a, 'b) t
  val empty : ('a, 'b) t
  val is_empty : ('a, 'b) t -> bool
  val match_node : node -> ('a, 'b) t -> ('a, 'b) context option * ('a, 'b) t
  val make : 'a node' list -> 'b edge' list -> ('a, 'b) t
  val labeled_nodes : ('a, 'b) t -> 'a node' Seq.t
end) =
struct
  include G

  let match_any g =
    match labeled_nodes g () with
    | Seq.Nil -> failwith "empty graph"
    | Seq.Cons ((v, _), _) ->
        let c, g' = match_node v g in
        (Option.get c, g')

  let cardinality g = Seq.length (labeled_nodes g)

  let rec ufold f u g =
    match g with
    | g when is_empty g -> u
    | _ ->
        let c, g' = match_any g in
        f c (ufold f u g')

  let gmap f = ufold (fun c g -> f c & g) empty
  let nmap f = gmap (fun (Context (p, n, l, s)) -> Context (p, n, f l, s))

  let rec dfs nodes graph =
    match nodes () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (v, vs) -> (
        match match_node v graph with
        | None, _ -> dfs vs graph
        | Some (Context (_, _, _, succs)), graph ->
            Seq.cons v
              (dfs (Seq.append (Seq.map snd (List.to_seq succs)) vs) graph))

  let rec bfs nodes graph =
    match nodes () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (v, vs) -> (
        match match_node v graph with
        | None, _ -> bfs vs graph
        | Some (Context (_, _, _, succs)), graph ->
            Seq.cons v
              (bfs (Seq.append vs (Seq.map snd (List.to_seq succs))) graph))

  type 'a tree = Br of 'a * 'a tree list [@@deriving show]

  let rec bf ps graph =
    match ps () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((v :: _ as p), ps) -> (
        match match_node v graph with
        | None, _ -> bf ps graph
        | Some (Context (_, _, _, succs)), graph ->
            Seq.cons p
              (bf
                 (Seq.append ps
                    (Seq.map (fun (_, x) -> x :: p) (List.to_seq succs)))
                 graph))
    | Seq.Cons (_, ps) -> bf ps graph

  let bft v = bf (Seq.return [ v ])

  let first p l =
    let filtered = Seq.filter p l in
    match filtered () with
    | Seq.Nil -> failwith "empty sequence"
    | Seq.Cons (a, _) -> a

  let esp s t graph =
    let paths = bft s graph in
    List.rev
      (first
         (fun path -> match path with v :: _ -> v == t | _ -> false)
         paths)

  let rec postorder (Br (v, ts)) =
    List.append (List.concat_map postorder ts) [ v ]

  let rec df nodes graph =
    match nodes () with
    | Seq.Nil -> ([], graph)
    | Seq.Cons (v, vs) -> (
        match match_node v graph with
        | None, _ -> df vs graph
        | Some (Context (_, _, _, succs)), graph ->
            let f, g1 = df (Seq.map snd (List.to_seq succs)) graph in
            let f', g2 = df vs g1 in
            (Br (v, f) :: f', g2))

  let dff nodes graph = fst (df nodes graph)
end

module InductiveGraph = Graph (struct
  open Dict

  type ('a, 'b) context' = {
    preds : 'b BinaryTrie.dict;
    succs : 'b BinaryTrie.dict;
    node : node;
    label : 'a;
  }

  let swap (x, y) = (y, x)

  let to_context { preds; succs; node; label } =
    Context
      ( List.map swap (BinaryTrie.to_list preds),
        node,
        label,
        List.map swap (BinaryTrie.to_list succs) )

  type ('a, 'b) t = InductiveGraph of ('a, 'b) context' BinaryTrie.dict

  let ( & ) (Context (preds, node, label, succs)) (InductiveGraph g) =
    let g =
      List.fold_left
        (fun graph (label, source) ->
          let ({ succs; _ } as ctx') =
            Option.get (BinaryTrie.lookup source graph)
          in
          BinaryTrie.insert fst source
            { ctx' with succs = BinaryTrie.insert fst node label succs }
            graph)
        g preds
    in
    let g =
      List.fold_left
        (fun graph (label, target) ->
          let ({ preds; _ } as ctx') =
            Option.get (BinaryTrie.lookup target graph)
          in
          BinaryTrie.insert fst target
            { ctx' with preds = BinaryTrie.insert fst node label preds }
            graph)
        g succs
    in
    InductiveGraph
      (BinaryTrie.insert fst node
         {
           preds = BinaryTrie.from_list (List.map swap preds);
           succs = BinaryTrie.from_list (List.map swap succs);
           node;
           label;
         }
         g)

  let empty = InductiveGraph BinaryTrie.empty
  let is_empty = function InductiveGraph BinaryTrie.Empty -> true | _ -> false

  let match_node n (InductiveGraph g) =
    let ctx, g' = BinaryTrie.remove n g in
    if Option.is_none ctx then (None, InductiveGraph g')
    else
      let { preds; succs; _ } = Option.get ctx in
      let pred_list = List.map fst (BinaryTrie.to_list preds) in
      let succ_list = List.map fst (BinaryTrie.to_list succs) in
      let g' =
        List.fold_left
          (fun graph source ->
            match BinaryTrie.lookup source graph with
            | None -> graph
            | Some ({ succs; _ } as ctx') ->
                BinaryTrie.insert fst source
                  { ctx' with succs = snd (BinaryTrie.remove n succs) }
                  graph)
          g' pred_list
      in
      let g' =
        List.fold_left
          (fun graph target ->
            match BinaryTrie.lookup target graph with
            | None -> graph
            | Some ({ preds; _ } as ctx') ->
                BinaryTrie.insert fst target
                  { ctx' with preds = snd (BinaryTrie.remove n preds) }
                  graph)
          g' succ_list
      in
      (Option.map to_context ctx, InductiveGraph g')

  let labeled_nodes (InductiveGraph g) =
    BinaryTrie.map (fun (n, { label; _ }) -> (n, label)) g

  let make nodes edges =
    let g =
      BinaryTrie.from_list
        (List.map
           (fun (node, label) ->
             ( node,
               {
                 preds = BinaryTrie.empty;
                 succs = BinaryTrie.empty;
                 node;
                 label;
               } ))
           nodes)
    in
    let g =
      List.fold_left
        (fun g (source, target, label) ->
          let ({ succs; _ } as source_ctx) =
            Option.get (BinaryTrie.lookup source g)
          in
          let source_ctx =
            { source_ctx with succs = BinaryTrie.insert fst target label succs }
          in
          let g = BinaryTrie.insert fst source source_ctx g in
          let ({ preds; _ } as target_ctx) =
            Option.get (BinaryTrie.lookup target g)
          in
          let target_ctx =
            { target_ctx with preds = BinaryTrie.insert fst source label preds }
          in
          let g = BinaryTrie.insert fst target target_ctx g in
          g)
        g edges
    in
    InductiveGraph g
end)

module Solution = struct
  let part_one = lazy "unimplemented"
  let part_two = lazy "unimplemented"
end

let%test_unit "tree" =
  let nodes = [ (0, ()); (1, ()); (2, ()); (3, ()); (4, ()) ] in
  let edges = [ (0, 1, ()); (0, 2, ()); (1, 3, ()); (1, 4, ()); (2, 0, ()) ] in
  let graph = InductiveGraph.make nodes edges in
  let tree = InductiveGraph.dff (Seq.return 0) graph in
  let order = InductiveGraph.postorder (List.nth tree 0) in
  let bfs_path =
    InductiveGraph.bfs (Seq.map fst (List.to_seq nodes)) graph |> List.of_seq
  in
  let dfs_path =
    InductiveGraph.dfs (Seq.map fst (List.to_seq nodes)) graph |> List.of_seq
  in
  let path = InductiveGraph.esp 0 4 graph in
  let open Base in
  [%test_eq: int list] [ 2; 4; 3; 1; 0 ] order;
  [%test_eq: int list] [ 0; 1; 2; 3; 4 ] bfs_path;
  [%test_eq: int list] [ 0; 2; 1; 4; 3 ] dfs_path;
  [%test_eq: int list] [ 0; 1; 4 ] path

let%test_unit "cyclic graph" =
  let nodes = List.init 7 (fun i -> (i + 1, ())) in
  let edges =
    List.map
      (fun (s, t) -> (s, t, ()))
      [
        (1, 2);
        (1, 3);
        (2, 1);
        (2, 4);
        (2, 5);
        (3, 1);
        (3, 6);
        (4, 2);
        (4, 7);
        (5, 2);
        (5, 7);
        (5, 6);
        (6, 3);
        (6, 5);
        (6, 7);
        (7, 4);
        (7, 5);
        (7, 6);
      ]
  in
  let graph = InductiveGraph.make nodes edges in
  let path = InductiveGraph.esp 1 6 graph in
  let open Base in
  [%test_eq: int list] [ 1; 3; 6 ] path
