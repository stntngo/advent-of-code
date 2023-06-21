open Angstrom

module Command = struct
  type target = Root | Up | Loc of string
  type content = Dir of string | File of int * string
  type t = List of content list | ChangeDir of target

  let dollar = char '$'

  let ws =
    skip_while (function
      | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
      | _ -> false)

  let change_dir =
    (dollar *> ws *> string "cd" *> ws
     *> take_till (function
          | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
          | _ -> false)
     >>| function
     | ".." -> Up
     | "/" -> Root
     | s -> Loc s)
    >>| fun t -> ChangeDir t

  let valid_char = function
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | '.' -> true
    | _ -> false

  let word = take_while1 valid_char

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let dir = string "dir" *> ws *> word >>| fun s -> Dir s
  let file = lift2 (fun size name -> File (size, name)) (integer <* ws) word

  let ls =
    dollar *> ws *> string "ls" *> ws *> sep_by1 (string "\n") (file <|> dir)
    >>| fun l -> List l

  let parse = change_dir <|> ls
end

module Directory = struct
  module P = Advent.Parser.Line (Command)

  let path_str p =
    let rec path_aux p =
      match p with [] -> "" | h :: t -> h ^ "/" ^ path_aux t
    in
    path_aux (List.rev p)

  type t = {
    path : string list;
    parent : t ref option;
    mutable subdirs : t ref list;
    mutable files : (int * string) list;
  }

  let make parent name =
    ref
      {
        path = name :: !parent.path;
        parent = Some parent;
        subdirs = [];
        files = [];
      }

  let x =
    10 |> string_of_int |> String.to_seq
    |> Seq.map (fun _ -> '*')
    |> String.of_seq

  let navigate commands =
    let root = ref { path = []; parent = None; subdirs = []; files = [] } in
    let rec nav_aux (commands : Command.t list) dir =
      match commands with
      | [] -> root
      | ChangeDir Up :: t -> nav_aux t (Option.get !dir.parent)
      | ChangeDir Root :: t -> nav_aux t root
      | ChangeDir (Loc p) :: t ->
          nav_aux t
            (List.find (fun d -> String.equal (List.hd !d.path) p) !dir.subdirs)
      | List c :: t ->
          List.iter
            (function
              | Command.Dir p -> !dir.subdirs <- make dir p :: !dir.subdirs
              | Command.File (s, f) -> !dir.files <- (s, f) :: !dir.files)
            c;
          nav_aux t dir
    in
    !(nav_aux commands root)

  let sizes d =
    let dirs = ref [] in
    let rec sum d =
      let folder_size =
        List.fold_left (fun acc (s, _) -> s + acc) 0 !d.files
        + List.fold_left (fun acc d -> acc + sum d) 0 !d.subdirs
      in
      dirs := (folder_size, !d.path) :: !dirs;
      folder_size
    in
    let _ = sum (ref d) in
    !dirs

  let parse = P.parse >>| navigate
end

module Solution = struct
  module P = Advent.Parser.Make (Directory)

  let directory = lazy (P.parse "input/day07")
  let fs_max = 70000000
  let unused_required = 30000000

  let part_one =
    lazy
      (Lazy.force directory |> Directory.sizes
      |> List.filter (fun (s, _) -> s <= 100000)
      |> List.fold_left (fun acc (s, _) -> acc + s) 0
      |> string_of_int)

  let part_two =
    lazy
      (let dirs = Lazy.force directory |> Directory.sizes in
       let used, _ = List.find (fun (_, p) -> List.length p == 0) dirs in
       let to_delete = unused_required - (fs_max - used) in
       let delete_size =
         dirs
         |> List.map (fun (s, _) -> s)
         |> List.filter (fun s -> s >= to_delete)
         |> function
         | [] -> invalid_arg "empty list"
         | x :: xs -> List.fold_left min x xs
       in
       delete_size |> string_of_int)
end
