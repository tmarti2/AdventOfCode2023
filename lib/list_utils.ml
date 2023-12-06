let iter_pairs ~f l =
  let rec aux = function
    | [] -> ()
    | [ _ ] -> assert false
    | x1 :: x2 :: tl ->
      f x1 x2;
      aux tl
  in
  aux l

let map_pairs ~f l =
  let rec aux acc = function
    | [] -> acc
    | [ _ ] -> assert false
    | x1 :: x2 :: tl -> aux (f x1 x2 :: acc) tl
  in
  aux [] l
