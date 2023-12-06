let ( << ) f g x = f (g x)

let int_list_to_int l =
  List.map string_of_int l |> String.concat "" |> int_of_string
