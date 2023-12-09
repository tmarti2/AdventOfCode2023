open Day

module Types = struct
  open Base

  type dir = L | R [@@deriving show]
  type path = dir list [@@deriving show]
  type node = string * (string * string) [@@deriving show]
  type input = path * node list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let right = char 'R' *> return R
  let left = char 'L' *> return L
  let path = many1 (right <|> left) <* end_of_line
  let dirs = enclosed '(' (both (word <* string ", ") word) ')'
  let node = both (word <* string " = ") dirs
  let input = both (path <* end_of_line) (sep_by1 end_of_line node)
end

module Solving = struct
  open Base
  open Misc_utils

  let create_map nodes =
    List.fold_left nodes
      ~init:(Map.empty (module String))
      ~f:(fun map (key, data) -> Map.add_exn ~key ~data map)

  let get_next h dir node =
    let l, r = Map.find_exn h node in
    match dir with L -> l | R -> r

  let follow_path ~stop map path start =
    Sequence.fold_until ~init:(start, 0)
      ~f:(fun (next, cpt) dir ->
        let next = get_next map dir next in
        if stop next then Stop (cpt + 1) else Continue (next, cpt + 1)
      )
      ~finish:(fun _ -> assert false)
      (Sequence.cycle_list_exn path)

  let follow_ghosts map path =
    Map.filter_keys map ~f:(end_with 'A')
    |> Map.keys
    |> List.map ~f:(follow_path ~stop:(end_with 'Z') map path)
    |> List.reduce_exn ~f:lcm

  let part1 ((path, nodes) : input) : output =
    let map = create_map nodes in
    follow_path ~stop:(Poly.( = ) "ZZZ") map path "AAA"

  let part2 ((path, nodes) : input) : output =
    let map = create_map nodes in
    follow_ghosts map path
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
