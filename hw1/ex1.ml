let rec merge ((a, b): int list * int list): int list =
  match a, b with
  | [], b -> b
  | a, [] -> a
  | a0::ax, b0::bx ->
      if a0 > b0 then
        a0::(merge (ax, (b0::bx)))
      else
        b0::(merge ((a0::ax), bx))
