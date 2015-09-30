type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

exception FreeVariable

let galculator (input: exp): float =
  let rec sigma (a: int) (b: int) (func: float -> float): float =
    if a == b then func (float_of_int a)
    else if a > b then 0.0
    else func (float_of_int a) +. sigma (a+1) b func
  in
  let rec integral (a: float) (b: float) (func: float -> float): float =
    let step = 0.1 in
    if a > b then -.integral b a func
    else if a +. step -. 0.00000001 > b then 0.0
    else step *. func a +. integral (a+.step) b func
  in
  let rec eval (input: exp) (x: float option): float =
    match input with
    | X -> begin
        match x with
        | Some value -> value
        | None -> raise FreeVariable
    end
    | INT num -> float_of_int num
    | REAL num -> num
    | ADD (left, right) -> (eval left x) +. (eval right x)
    | SUB (left, right) -> (eval left x) -. (eval right x)
    | MUL (left, right) -> (eval left x) *. (eval right x)
    | DIV (left, right) -> (eval left x) /. (eval right x)
    | SIGMA (a0, b0, body) ->
        let a = eval a0 x in
        let b = eval b0 x in
        sigma (int_of_float a) (int_of_float b) (fun h -> eval body (Some h))
    | INTEGRAL (a0, b0, body) ->
        let a = eval a0 x in
        let b = eval b0 x in
        integral a b (fun h -> eval body (Some h))
  in
  eval input None
