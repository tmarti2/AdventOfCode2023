open Day

module Types = struct
  type interval = { start : int; len : int } [@@deriving show]
  type range = interval * interval [@@deriving show]
  type input = { seeds : int list; passes : range list list } [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let seeds = string "seeds: " *> sep_by1 space integer

  let range =
    lift3
      (fun dst src len -> { start = src; len }, { start = dst; len })
      (integer <* space) (integer <* space) integer

  let map_name s = string s *> string " map:" *> end_of_line
  let sep = end_of_line *> end_of_line
  let map s = sep *> map_name s *> sep_by1 end_of_line range

  let passes =
    list
      [
        map "seed-to-soil";
        map "soil-to-fertilizer";
        map "fertilizer-to-water";
        map "water-to-light";
        map "light-to-temperature";
        map "temperature-to-humidity";
        map "humidity-to-location";
      ]

  let input = lift2 (fun seeds passes -> { seeds; passes }) seeds passes
end

module Solving = struct
  open Base

  (* Return a couple of interval (todo, done). Intersection between src and dst
     is converted using conv. *)
  let split src dst conv =
    (* src = [a; b] *)
    let a = src.start in
    let b = src.start + src.len in
    (* dst = [x; y] *)
    let x = dst.start in
    let y = dst.start + dst.len in
    let c = conv.start in
    if a > y || b < x then
      (* x y a b || a b x y --> nothing inside, everything todo, no match *)
      Some src, None
    else if a >= x && b <= y then
      (* x a b y --> everything inside, nothing todo, match [a; b] *)
      None, Some { src with start = c + (a - x) }
    else if a >= x then
      (* x a y b --> partially inside, todo [y; b], match [a; y] *)
      Some { start = y; len = b - y }, Some { start = a - x + c; len = y - a }
    else
      (* a x b y --> partially inside, todo [a; x], match [x; b] *)
      Some { src with len = x - a }, Some { conv with len = b - x }

  let find_dst_interval src ranges =
    let rec aux acc ranges src =
      match ranges with
      | [] -> src :: acc
      | (dst, conv) :: tl -> begin
        match split src dst conv with
        | None, None -> assert false
        | None, Some intv -> intv :: acc
        | Some todo, None -> aux acc tl todo
        | Some todo, Some intv -> aux (intv :: acc) tl todo
      end
    in
    List.map ~f:(aux [] ranges) src |> List.concat

  let part_aux input seeds =
    List.fold_left input.passes ~init:seeds ~f:find_dst_interval
    |> List.min_elt ~compare:(fun i1 i2 -> compare i1.start i2.start)
    |> Option.value_exn
    |> fun i -> i.start

  let part1 (input : input) : output =
    let f seed = { start = seed; len = 1 } in
    List.map input.seeds ~f |> part_aux input

  let part2 (input : input) : output =
    let f start len = { start; len } in
    List_utils.map_pairs input.seeds ~f |> part_aux input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
