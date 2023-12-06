let call ~f (a,b) = f a b

let call_map ~f (a, b) = List.map2 f a b

let map ~f (a,b) = (f a, f b)