open Day

module Types = struct
  type set = { red : int; green : int; blue : int } [@@deriving show]
  type game = { id : int; sets : set list } [@@deriving show]
  type input = game list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing
  open Base

  type color = Blue of int | Red of int | Green of int

  let blue = integer <* string " blue" >>| fun i -> Blue i
  let red = integer <* string " red" >>| fun i -> Red i
  let green = integer <* string " green" >>| fun i -> Green i
  let color = blue <|> red <|> green

  let to_set color_list =
    let init = { red = 0; green = 0; blue = 0 } in
    List.fold_left color_list ~init ~f:(fun acc c ->
        match c with
        | Red i -> { acc with red = i }
        | Green i -> { acc with green = i }
        | Blue i -> { acc with blue = i }
    )

  let set = sep_by1 (string ", ") color >>| to_set
  let sets = sep_by1 (string "; ") set
  let id = string "Game " *> integer <* string ": "
  let game = lift2 (fun id sets -> { id; sets }) id sets
  let input = sep_by1 end_of_line game
end

module Solving = struct
  open Base

  let exceed (mr, mg, mb) set = set.red > mr || set.green > mg || set.blue > mb

  let filter_possible max games =
    List.filter_map games ~f:(fun game ->
        if List.exists game.sets ~f:(exceed max) then None else Some game
    )

  let limit acc set =
    {
      red = max acc.red set.red;
      green = max acc.green set.green;
      blue = max acc.blue set.blue;
    }

  let find_mins games =
    let init = { red = 0; green = 0; blue = 0 } in
    List.map games ~f:(fun game -> List.fold_left game.sets ~init ~f:limit)

  let part1 (input : input) : output =
    filter_possible (12, 13, 14) input
    |> List.sum ~f:(fun g -> g.id) (module Int)

  let part2 (input : input) : output =
    find_mins input
    |> List.sum ~f:(fun s -> s.red * s.green * s.blue) (module Int)
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
