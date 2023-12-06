open Day

module Types = struct
  type input = int list * int list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let time : int list t = string "Time:" *> many1 (leading space integer)
  let dist : int list t = string "Distance:" *> many1 (leading space integer)
  let input = both (time <* end_of_line) dist
end

module Solving = struct
  open Base

  let find_all time distance =
    let rec aux acc speed =
      if speed >= time then acc
      else if speed * (time - speed) > distance then aux (acc + 1) (speed + 1)
      else aux acc (speed + 1)
    in
    aux 0 0

  let part1 (input : input) : output =
    Pairs_utils.call_map ~f:find_all input |> List.reduce_exn ~f:( * )

  let part2 (input : input) : output =
    Pairs_utils.map ~f:Misc_utils.int_list_to_int input
    |> Pairs_utils.call ~f:find_all
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
