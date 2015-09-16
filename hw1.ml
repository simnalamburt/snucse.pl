(* Exercise 1 *)
let rec merge (a: int list) (b: int list): int list =
  match a, b with
  | [], b -> b
  | a, [] -> a
  | a0::ax, b0::bx ->
      if a0 > b0 then
        a0::(merge ax (b0::bx))
      else
        b0::(merge (a0::ax) bx)

(* Exercise 2 *)
let sigma (a: int) (b: int) (f: int -> int): int =
  let sum = ref 0 in
  for i = a to b do
    sum := !sum + (f i)
  done;
  !sum

(* Exercise 3 *)
let rec iter (n: int) (f: 'a -> 'a): ('a -> 'a) =
  if n <= 0 then
    function x -> x
  else
    let compose f g x = f(g(x)) in
    compose f (iter (n-1) f)

(* Exercise 4 *)
type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr

let rec eval (form: formula): bool =
  let rec calc (exp: expr): int =
    match exp with
    | NUM(i) -> i
    | PLUS(e1, e2) -> (calc e1) + (calc e2)
    | MINUS(e1, e2) -> (calc e1) - (calc e2)
  in
  match form with
  | TRUE -> true
  | FALSE -> false
  | NOT(f) -> not (eval f)
  | ANDALSO(f1, f2) -> (eval f1) && (eval f2)
  | ORELSE(f1, f2) -> (eval f1) || (eval f2)
  | IMPLY(f1, f2) -> not (eval f1) || (eval f2)
  | LESS(e1, e2) -> (calc e1) < (calc e2)

(* Exercise 5 *)
type nat = ZERO
         | SUCC of nat

let rec natadd (left: nat) (right: nat): nat =
  match right with
  | ZERO -> left
  | SUCC(r1) -> natadd (SUCC(left)) r1

let rec natmul (left: nat) (right: nat): nat =
  match right with
  | ZERO -> ZERO
  | SUCC(r1) -> natadd (natmul left r1) left
