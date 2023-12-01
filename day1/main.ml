open Aoclib

module Types = struct
  type input = string list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let input = sep_by end_of_line line
end

module Solving = struct
  open Base

  type t = Int of int | Str of int [@@deriving show]

  let none_f ~f v = match v with None -> f () | Some _ -> v

  let convert s =
    let size = String.length s in
    let check part pos res =
      let len = String.length part in
      if pos + len >= size then None
      else
        let s' = String.sub s ~pos:(pos + 1) ~len in
        if String.equal s' part then Some (Str res) else None
    in
    String.foldi s ~init:[] ~f:(fun i acc c ->
        let res =
          match c with
          | c when Char.is_digit c -> Some (Int (Char.get_digit_exn c))
          | 'o' -> check "ne" i 1
          | 't' -> none_f ~f:(fun () -> check "hree" i 3) (check "wo" i 2)
          | 'f' -> none_f ~f:(fun () -> check "ive" i 5) (check "our" i 4)
          | 's' -> none_f ~f:(fun () -> check "even" i 7) (check "ix" i 6)
          | 'e' -> check "ight" i 8
          | 'n' -> check "ine" i 9
          | _ -> None
        in
        match res with None -> acc | Some v -> v :: acc)
    |> List.rev

  let is_int = function Int _ -> true | Str _ -> false
  let get = function Int i | Str i -> i

  let part_aux ~f input =
    let f l =
      let x = f l in
      let y = List.rev l |> f in
      match (x, y) with
      | Some i1, Some i2 -> (get i1 * 10) + get i2
      | _ -> 0
    in
    List.map ~f:convert input |> List.sum ~f (module Int)

  let part1 (input : input) : output = part_aux ~f:(List.find ~f:is_int) input
  let part2 (input : input) : output = part_aux ~f:List.hd input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
