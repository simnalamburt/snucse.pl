type item = string
type tree = LEAF of item
          | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft (loc: location): location =
  match loc with
  | LOC (_, TOP) -> raise (NOMOVE "")
  | LOC (_, HAND([], _, _)) -> raise (NOMOVE "")
  | LOC (t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))

let goRight (loc: location): location =
  match loc with
  | LOC (_, TOP) -> raise (NOMOVE "")
  | LOC (_, HAND(_, _, [])) -> raise (NOMOVE "")
  | LOC (t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))

let goUp (loc: location): location =
  match loc with
  | LOC (_, TOP) -> raise (NOMOVE "")
  | LOC (t, HAND(left, up, right)) -> LOC(NODE (List.rev left@[t]@right), up)

let goDown (LOC(t, up): location): location =
  match t with
  | LEAF _
  | NODE [] -> raise (NOMOVE "")
  | NODE (c0::children) -> LOC(c0, HAND([], up, children))
