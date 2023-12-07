open Day

module Types = struct
  open Base

  type card = A | K | Q | J | Num of int [@@deriving show, sexp_of]
  type hand = card list [@@deriving show]
  type input = (hand * int) list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let card =
    satisfy not_nl >>| function
    | 'T' -> Num 10
    | 'A' -> A
    | 'K' -> K
    | 'Q' -> Q
    | 'J' -> J
    | d when is_digit d -> Num (Base.Char.get_digit_exn d)
    | _ -> assert false

  let hand = list [ card; card; card; card; card ]
  let line = both (hand <* space) (integer <* end_of_line)
  let input = many1 line
end

module Solving = struct
  open Base

  type value = Five | Four | Three | Full | Pair | TwoPair | High
  [@@deriving show]

  let to_int_card ~joker = function
    | A -> 14
    | K -> 13
    | Q -> 12
    | J when joker -> 1
    | J -> 11
    | Num i -> i

  let to_int_value = function
    | Five -> 7
    | Four -> 6
    | Full -> 5
    | Three -> 4
    | TwoPair -> 3
    | Pair -> 2
    | High -> 1

  let compare_card ~joker c1 c2 =
    compare (to_int_card ~joker c1) (to_int_card ~joker c2)

  let compare_hand ~get ~joker (h1, _) (h2, _) =
    match get h1, get h2 with
    | v1, v2 when Poly.(v1 = v2) -> List.compare (compare_card ~joker) h1 h2
    | v1, v2 -> compare (to_int_value v1) (to_int_value v2)

  let count_cards hand =
    let f = function None -> 1 | Some i -> i + 1 in
    List.map hand ~f:(to_int_card ~joker:false)
    |> List.fold_left ~init:(Map.empty (module Int)) ~f:(Map.update ~f)
    |> Map.data

  let get_value hand =
    match count_cards hand with
    | [] -> assert false
    | [ 5 ] -> Five
    | [ 4; 1 ] | [ 1; 4 ] -> Four
    | [ 3; 2 ] | [ 2; 3 ] -> Full
    | [ 3; 1; 1 ] | [ 1; 3; 1 ] | [ 1; 1; 3 ] -> Three
    | [ 2; 2; 1 ] | [ 2; 1; 2 ] | [ 1; 2; 2 ] -> TwoPair
    | l -> if List.exists l ~f:(( = ) 2) then Pair else High

  let get_upgraded_value h =
    let nbj = List.count ~f:(Poly.( = ) J) h in
    let value = get_value h in
    if nbj = 0 then value
    else
      match value with
      | Five -> Five
      | Four -> Five
      | Full -> Five
      | Three -> Four
      | TwoPair when nbj = 1 -> Full
      | TwoPair -> Four
      | Pair -> Three
      | High -> Pair

  let part_aux ~get ~joker input =
    List.sort ~compare:(compare_hand ~get ~joker) input
    |> List.mapi ~f:(fun i (_, bid) -> (i + 1) * bid)
    |> List.sum ~f:Fn.id (module Int)

  let part1 (input : input) : output =
    part_aux ~get:get_value ~joker:false input

  let part2 (input : input) : output =
    part_aux ~get:get_upgraded_value ~joker:true input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
