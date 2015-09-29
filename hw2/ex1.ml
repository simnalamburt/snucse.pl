type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val (input: crazy2): int =
  match input with
  | NIL -> 0
  | ZERO rest -> 2*(crazy2val rest)
  | ONE  rest -> 1 + 2*(crazy2val rest)
  | MONE rest -> -1 + 2*(crazy2val rest)
