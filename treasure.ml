type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map =
  | End of treasure
  | Branch of map * map
  | Guide of string * map

(* TODO : implement *)
let getReady map = [Bar; Bar; Bar]


(* test codes below *)
let explore map =
  let rec string_of_key key =
    match key with
    | Bar -> "-"
    | Node(k1,k2) -> "(" ^ string_of_key k1 ^ "," ^ string_of_key k2 ^ ")"
  in
  print_endline (
    "{" ^
    String.concat ", " (List.map string_of_key (getReady map)) ^
    "}")
