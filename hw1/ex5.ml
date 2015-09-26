type nat = ZERO
         | SUCC of nat

let rec natadd ((left, right): nat * nat): nat =
  match right with
  | ZERO -> left
  | SUCC r1 -> natadd ((SUCC(left)), r1)

let rec natmul ((left, right): nat * nat): nat =
  match right with
  | ZERO -> ZERO
  | SUCC r1 -> natadd ((natmul (left, r1)), left)
