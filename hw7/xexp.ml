(*
 * SNU 4190.310 Programming Languages 
 * Xexp Language Definition and Interpreter
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

type xexp = 
  | Num of int
  | Var of string
  | Fn of string * xexp
  | App of xexp * xexp
  | If of xexp * xexp * xexp
  | Equal of xexp * xexp
  | Raise of xexp
  | Handle of xexp * int * xexp

type value = 
  | N of int                (* Integer *)
  | B of bool               (* Boolean *)
  | C of closure            (* Closure *)
and closure = string * xexp * env
and env = string -> value

type result = 
  | Val of value  (* Value *)
  | Exn of int    (* Exception *)

exception Unhandled of int
exception RunError of string
exception TypeError of string

let emptyEnv = (fun x -> raise (RunError ("unbound id : " ^ x)))

let bind e x v = (fun y -> if y = x then v else e y)
  
let getInt = function 
  | N n -> n 
  | _ -> raise (TypeError "not an int")

let getBool = function
  | B b -> b
  | _ -> raise (TypeError "not a bool")

let getClosure = function 
  | C c -> c 
  | _ -> raise (TypeError "not a function")

let rec eval env exp = 
  match exp with
  | Num n -> Val (N n)
  | Var x -> Val (env x)
  | Fn (x, e) -> Val (C (x, e, env))
  | App (e1, e2) ->
    (* e2 must be evaluated only if e1 is evaluated to a value *)
    (match eval env e1 with
    | Val v1 -> 
      let (x, e_body, env') = getClosure v1 in
      (match eval env e2 with
      | Val v2 -> eval (bind env' x v2) e_body
      | Exn n -> Exn n)
    | Exn n -> Exn n)
  | If (e1, e2, e3) ->
    (match eval env e1 with
    | Val v -> if getBool v then eval env e2 else eval env e3
    | Exn n -> Exn n)
  | Equal (e1, e2) -> 
    (* e2 must be evaluated only if e1 is evaluated to a value *)
    (match eval env e1 with
    | Val v1 -> 
      (match eval env e2 with
      | Val v2 -> Val (B (getInt v1 = getInt v2))
      | Exn n -> Exn n)
    | Exn n -> Exn n)
  | Raise e ->
    (match eval env e with
    | Val v -> Exn (getInt v)
    | Exn n -> Exn n)
  | Handle (e1, n, e2) ->
    (* e2 must be evaluated only if e1 is evaluated to an exception *)
    (match eval env e1 with
    | Val v1 -> Val v1 
    | Exn n' -> if n = n' then eval env e2 else Exn n')

let run : xexp -> result = fun exp -> eval emptyEnv exp 

let ps = print_string
let nl = print_newline
let indent i =
  let rec iter = function 
    | 0 -> ()
    | n -> ps " "; iter (n-1)
  in  
  nl (); iter i
  
let rec pp i exp = 
  match exp with
  | Num n -> print_int n
  | Var s -> ps s
  | Fn (x, e) -> ps ("fn " ^ x ^ " => "); pp i e
  | App (e, e') -> ps "("; pp i e; ps ") ("; pp i e'; ps ")"
  | If (e1, e2, e3)-> 
    indent (i+1); ps "if "; pp i e1; ps " then ";
    indent (i+2); pp (i+2) e2;
    indent (i+1); ps "else";
    indent (i+2); pp (i+2) e3;
    nl ()
  | Equal (e1, e2) -> ps "("; pp i e1; ps " = "; pp i e2; ps ")"
  | Raise e -> ps "raise ("; pp i e ; ps ")"
  | Handle (e1, n, e2) -> 
    ps "("; pp i e1; ps ") ";
    ps ("handle " ^ string_of_int n ^ " ("); pp i e2; ps ")"

let print = pp 0

let rec is_sugarless = function
  | Num _ | Var _ -> true
  | Fn (_, e) -> is_sugarless e
  | App (e1, e2) | Equal (e1, e2) -> is_sugarless e1 && is_sugarless e2
  | If (e1, e2, e3) -> is_sugarless e1 && is_sugarless e2 && is_sugarless e3
  | Raise _ | Handle _ -> false
