open Aoclib.Day

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

  type t = Int of int | Str of int

  let none_f ~f v = match v with None -> f () | Some _ -> v
  let get = function Int i | Str i -> i

  exception Found of t option

  let search ~f s pos c =
    let size = String.length s in
    let check part pos res =
      let len = String.length part in
      if pos + len >= size then None
      else
        let s' = String.sub s ~pos:(pos + 1) ~len in
        if String.equal s' part then Some (Str res) else None
    in
    if f c then
      match c with
      | c when Char.is_digit c -> Some (Int (Char.get_digit_exn c))
      | 'o' -> check "ne" pos 1
      | 't' -> none_f ~f:(fun () -> check "hree" pos 3) (check "wo" pos 2)
      | 'f' -> none_f ~f:(fun () -> check "ive" pos 5) (check "our" pos 4)
      | 's' -> none_f ~f:(fun () -> check "even" pos 7) (check "ix" pos 6)
      | 'e' -> check "ight" pos 8
      | 'n' -> check "ine" pos 9
      | _ -> None
    else None

  let find_first ~f s = String.find_mapi s ~f:(search ~f s)

  let find_last ~f s =
    let len = String.length s in
    try
      for i = 0 to len - 1 do
        let id = len - 1 - i in
        match search ~f s id s.[id] with
        | None -> ()
        | Some v -> raise (Found (Some v))
      done;
      assert false
    with Found v -> v

  let part_aux ~f input =
    let f s =
      match (find_first ~f s, find_last ~f s) with
      | Some i1, Some i2 -> (get i1 * 10) + get i2
      | _ -> 0
    in
    List.sum ~f (module Int) input

  let part1 (input : input) : output = part_aux ~f:Char.is_digit input
  let part2 (input : input) : output = part_aux ~f:(fun _ -> true) input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
