(*
 * SNU 4190.310 Programming Languages 
 * M0 Language Definition and Interpreter
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

type mexp = 
  | Num of int
  | Var of id
  | Fn of id * mexp
  | App of mexp * mexp
  | Rec of id * id * mexp
  | Ifz of mexp * mexp * mexp
  | Add of mexp * mexp
  | Pair of mexp * mexp      (* (e, e) *)
  | Fst of mexp            (*   e.1  *)
  | Snd of mexp            (*   e.2  *)
and id = string

type value = 
  | N of int
  | P of value * value
  | C of closure
and closure = fexpr * env
and fexpr = Fun of id * mexp | RecFun of id * id * mexp
and env = id -> value

exception RunError of string
exception TypeError of string

let bind e x v = (fun y -> if y = x then v else e y)
  
let getNum = function 
  | N n -> n 
  | _ -> raise (TypeError "not an int")

let getPair = function 
  | (P (a,b)) -> (a, b) 
  | _ -> raise (TypeError "not a pair")

let getClosure = function 
  | (C c) -> c 
  | _ -> raise (TypeError "not a function")

let rec eval env exp = 
  match exp with
  | Num n -> N n
  | Var x -> env x
  | Fn (x, e) -> C (Fun (x, e), env)
  | Rec (f, x, e) -> C (RecFun (f, x, e), env)
  | App (e1, e2) ->
    let v2 = eval env e2 in
    let v1 = eval env e1 in
    let (c, env') = getClosure v1 in
    (match c with 
    | Fun (x, e) -> eval (bind env' x v2) e
    | RecFun (f, x, e) ->  
      let env'' = bind env' x v2 in
      let env''' = bind env'' f v1 in
      eval env''' e)
  | Ifz (e1, e2, e3) ->
    let v1 = eval env e1 in
    if getNum v1 = 0 then eval env e2 else eval env e3
  | Add (e1, e2) -> 
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    N (getNum v1 + getNum v2)
  | Pair (e1, e2) -> 
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    P (v1, v2)
  | Fst e -> 
    let v = eval env e in
    fst (getPair v)
  | Snd e -> 
    let v = eval env e in
    snd (getPair v)

let emptyEnv = (fun x -> raise (RunError ("unbound id: " ^ x)))

let run : mexp -> value = fun exp -> eval emptyEnv exp

let ps = print_string
let nl = print_newline
let indent i =
  let rec iter = function 
    | 0 -> ()
    | n -> ps " "; iter (n-1)
  in  
  nl (); iter i
  
let rec pp n = function 
  | Num i -> print_int i
  | Var s -> ps s
  | Fn (x, e) -> ps ("fn "^ x ^" => "); 
    (match e with
    | Ifz _ -> indent (n+1); pp (n+1) e
    | _ -> pp n e)
  | App (e, e') -> ps "("; pp n e; ps ") ("; pp n e'; ps ")"
  | Ifz (e1, e2, e3)-> 
    ps "ifzero "; pp n e1; ps " then ";
    indent (n+1); pp (n+1) e2;
    indent (n); ps "else";
    indent (n+1); pp (n+1) e3
  | Pair (e1, e2) -> ps "("; pp n e1; ps ", "; pp n e2; ps ")"
  | Fst e -> pp n e; ps ".1"
  | Snd e -> pp n e; ps ".2"
  | Add (e1, e2) -> ps "("; pp n e1; ps " + "; pp n e2; ps ")"
  | Rec (f, x, e) -> ps ("rec "^f^" "^x^" => "); pp n e

let print = pp 0

