open Day
open Array_utils

module Types = struct
  type t = Num of int | Symb of char | Dot [@@deriving show]
  type input = t array array [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let num = digit >>| fun i -> Num i
  let dot = char '.' *> return Dot
  let symb = satisfy not_nl >>| fun c -> Symb c
  let line = many (num <|> dot <|> symb) >>| Array.of_list
  let lines = sep_by1 end_of_line line
  let input = lines >>| Array.of_list
end

module Solving = struct
  open Base

  module Coords = struct
    type t = int * int [@@deriving sexp, compare, hash]
  end

  module Coords_set = struct
    include Coords
    include Base.Comparator.Make (Coords)
  end

  let touch_symb map p =
    let f = function Symb _ -> true | _ -> false in
    exist_neightbor_map ~f map p

  let all_gears map p =
    let f = function Symb '*' -> true | _ -> false in
    filter_cell_map ~f map p |> Set.of_list (module Coords_set)

  let find_touch map =
    Array.foldi map ~init:[] ~f:(fun y acc line ->
        let acc, num, touch =
          Array.foldi line ~init:(acc, 0, false)
            ~f:(fun x (acc, num, touch) cell ->
              match cell with
              | Dot ->
                let acc = if num <> 0 && touch then num :: acc else acc in
                acc, 0, false
              | Num i when touch -> acc, (num * 10) + i, touch
              | Num i -> acc, (num * 10) + i, touch_symb map (x, y)
              | Symb _ ->
                let acc = if num <> 0 then num :: acc else acc in
                acc, 0, true
          )
        in
        if num <> 0 && touch then num :: acc else acc
    )

  module Hashtbl = Stdlib.Hashtbl

  let find_gears map =
    let h = Hashtbl.create 7 in
    let append p num =
      match Hashtbl.find_opt h p with
      | None -> Hashtbl.add h p [ num ]
      | Some nums -> Hashtbl.replace h p (num :: nums)
    in
    Array.iteri map ~f:(fun y line ->
        Array.foldi line
          ~init:(0, Set.empty (module Coords_set))
          ~f:(fun x (num, gears) cell ->
            match cell with
            | Dot | Symb _ ->
              if num <> 0 then Set.iter gears ~f:(fun p -> append p num);
              0, Set.empty (module Coords_set)
            | Num i when x = Array.length line - 1 ->
              let g' = Set.union gears (all_gears map (x, y)) in
              Set.iter g' ~f:(fun p -> append p ((num * 10) + i));
              0, Set.empty (module Coords_set)
            | Num i -> (num * 10) + i, Set.union (all_gears map (x, y)) gears
          )
        |> ignore
    );
    Hashtbl.fold
      (fun _ nums acc -> if List.length nums > 1 then nums :: acc else acc)
      h []

  let part1 (input : input) : output =
    find_touch input |> List.sum ~f:Fn.id (module Int)

  let part2 (input : input) : output =
    find_gears input |> List.sum ~f:(List.reduce_exn ~f:( * )) (module Int)
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
