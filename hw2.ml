(* Exercise 1 *)
type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val (input: crazy2): int =
  match input with
  | NIL -> 0
  | ZERO(rest) -> 2*(crazy2val rest)
  | ONE(rest) -> 1 + 2*(crazy2val rest)
  | MONE(rest) -> -1 + 2*(crazy2val rest)

(* Exercise 2 *)
let rec crazy2add ((left, right): crazy2 * crazy2): crazy2 =
  match left, right with
  | _, NIL -> left
  | NIL, _ -> right
  | ZERO l0, ZERO r0 -> ZERO (crazy2add (l0, r0))
  | ZERO l0, ONE r0  -> ONE  (crazy2add (l0, r0))
  | ZERO l0, MONE r0 -> MONE (crazy2add (l0, r0))
  | ONE  l0, ZERO r0 -> ONE  (crazy2add (l0, r0))
  | ONE  l0, ONE r0  -> ZERO (crazy2add (crazy2add (l0, r0), ONE NIL))
  | ONE  l0, MONE r0 -> ZERO (crazy2add (l0, r0))
  | MONE l0, ZERO r0 -> MONE (crazy2add (l0, r0))
  | MONE l0, ONE r0  -> ZERO (crazy2add (l0, r0))
  | MONE l0, MONE r0 -> ZERO (crazy2add (crazy2add (l0, r0), MONE NIL))

(* Exercise 3 *)

(* Exercise 4 *)

(* Exercise 5 *)

(* Exercise 6 *)
