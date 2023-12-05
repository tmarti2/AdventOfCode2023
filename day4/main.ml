open Aoclib.Day

module Types = struct
  type card = { id : int; left : int list; right : int list } [@@deriving show]
  type input = card list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let num_list = sep_by1 space (leading space integer)
  let id = string "Card " *> (leading space integer) <* string ": "

  let card =
    lift3
      (fun id left right -> { id; left; right })
      id (num_list <* (string " | ")) num_list

  let input = sep_by1 end_of_line card
end

module Solving = struct
  open Base

  let matching card =
    let left = Set.of_list (module Int) card.left in
    let right = Set.of_list (module Int) card.right in
    Set.inter left right |> Set.length |> fun l -> (1, l)

  let compute cards =
    Array.foldi ~init:0 cards ~f:(fun i acc (nb_cards, wins) ->
        let cards_won = if wins <> 0 then nb_cards else 0 in
        for x = i + 1 to i + wins do
          let nb_cards', wins' = cards.(x) in
          cards.(x) <- (nb_cards' + cards_won, wins')
        done;
        acc + nb_cards
    )

  let part1 (input : input) : output =
    let f (_, n) = if n = 0 then 0 else 2 ** (n - 1) in
    List.map ~f:matching input |> List.sum ~f (module Int)

  let part2 (input : input) : output =
    List.map ~f:matching input |> Array.of_list |> compute
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
