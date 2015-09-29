type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro (input: metro): bool =
  let rec check (input: metro) (rules: string list): bool =
    match input with
    | STATION name -> List.exists ((=) name) rules
    | AREA (name, m0) -> check m0 (name::rules)
    | CONNECT (m0, m1) -> (check m0 rules) && (check m1 rules)
  in
  check input []
