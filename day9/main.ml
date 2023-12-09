open Day

module Types = struct
  open Base

  type input = int list list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let line = many1 (leading space neg_integer)
  let input = sep_by1 end_of_line line
end

module Solving = struct
  open Base

  let diff l =
    let rec aux acc todo =
      match todo with
      | [] -> assert false
      | [ _ ] -> acc
      | e1 :: e2 :: tl -> aux ((e2 - e1) :: acc) (e2 :: tl)
    in
    aux [] l |> List.rev

  let until_zeros l =
    let rec aux acc todo =
      if List.for_all ~f:(( = ) 0) todo then acc
      else
        let next = diff todo in
        aux (next :: acc) next
    in
    aux [ l ] l

  let add_tails ll = List.sum (module Int) ~f:List.last_exn ll

  let sub_heads ll =
    List.fold_left ll ~init:0 ~f:(fun acc l -> List.hd_exn l - acc)

  let part_aux ~f input =
    List.map ~f:until_zeros input |> List.sum ~f (module Int)

  let part1 (input : input) : output = part_aux ~f:add_tails input
  let part2 (input : input) : output = part_aux ~f:sub_heads input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
