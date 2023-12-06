open Base
open Misc_utils

let pp_array pp_elem fmt s =
  let pp_sep ppf () = Stdlib.Format.fprintf ppf "|" in
  Stdlib.Array.to_seq s |> Stdlib.Format.pp_print_seq ~pp_sep pp_elem fmt

let valid a x = x >= 0 && x < Array.length a
let valid_map map (x, y) = valid map y && valid map.(y) x
let get a x = if valid a x then Some a.(x) else None
let get_map map (x, y) = if valid_map map (x, y) then Some map.(y).(x) else None
let adj x = [ x - 1; x + 1 ]

let adj_map (x, y) =
  [
    x - 1, y - 1;
    x - 1, y;
    x - 1, y + 1;
    x, y - 1;
    x, y + 1;
    x + 1, y - 1;
    x + 1, y;
    x + 1, y + 1;
  ]

let neightbors_cells a p = List.filter (adj p) ~f:(valid a)
let neightbors_cells_map a p = List.filter (adj_map p) ~f:(valid_map a)
let neightbors a p = List.filter_map (adj p) ~f:(get a)
let neightbors_map a p = List.filter_map (adj_map p) ~f:(get_map a)

let filter_aux ~get ~adj ~f a p =
  let f = Option.value_map ~default:false ~f in
  List.filter ~f:(f << get a) (adj a p)

let filter_cells ~f a p = filter_aux ~get ~adj:neightbors_cells ~f a p

let filter_cell_map ~f a p =
  filter_aux ~get:get_map ~adj:neightbors_cells_map ~f a p

let filter_neightbors ~f a p = List.filter ~f (neightbors a p)
let filter_neightbors_map ~f a p = List.filter ~f (neightbors_map a p)
let exist_neightbor ~f a p = List.exists ~f (neightbors a p)
let exist_neightbor_map ~f a p = List.exists ~f (neightbors_map a p)

(* let find_aux ~f ~adj ~get a p = let f = Option.map ~f in List.find_map (adj
   p) ~f:(f << get a)

   let find_neightbor a p = find_aux ~adj ~get a p let find_neightbor_map a p =
   find_aux ~adj:adj_map ~get:get_map a p *)
