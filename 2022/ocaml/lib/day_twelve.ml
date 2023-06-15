module type Graph = sig
  type node

  type t

  val show_node : node -> string

  val equal : node -> node -> bool

  val compare : node -> node -> int

  val neighbors : t -> node -> node list
end

module Search (G : Graph) = struct
  include G

  module S = Set.Make (struct
    type t = G.node

    let compare = G.compare
  end)

  let paths graph start target =
    let visited = ref S.empty in
    let rec paths_aux target node =
      let () = print_endline "-------" in
      let () = print_endline (string_of_int (S.cardinal !visited)) in
      let () = visited := S.add node !visited in
      if equal node target then [ [ target ] ]
      else
        let n =
          neighbors graph node |> List.filter (fun n -> not (S.mem n !visited))
        in
        let () = node |> show_node |> print_endline in
        let () = n |> List.map show_node |> String.concat "" |> print_endline in
        let p =
          n
          |> List.map (paths_aux target)
          |> List.flatten
          |> List.map (List.cons node)
        in
        let () =
          p
          |> List.map (fun n -> List.map show_node n)
          |> List.map (fun n -> String.concat "" n)
          |> String.concat "\n\n" |> print_endline
        in
        let () = visited := S.remove node !visited in
        p
    in
    paths_aux target start
end

module DummyGraph = struct
  type node = char [@@deriving show]

  type t = unit

  let equal = Char.equal

  let compare = Char.compare

  let neighbors () n =
    match n with
    | 'A' -> [ 'E'; 'B' ]
    | 'B' -> [ 'C' ]
    | 'C' -> [ 'D'; 'F'; 'B' ]
    | 'D' -> []
    | 'E' -> [ 'C'; 'B' ]
    | 'F' -> []
    | _ -> failwith "unknown node"
end

module Solution = struct
  module DFS = Search (DummyGraph)

  let part_one =
    lazy
      (DFS.paths () 'A' 'D' |> List.map List.to_seq |> List.map String.of_seq
     |> String.concat "\n\n")

  let part_two = lazy "unimplemented"
end
