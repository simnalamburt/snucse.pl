(*
 * SNU 4190.310 Programming Languages 2015 Fall
 * M Language Definition and Interpreter
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)


(* Definition of M's syntax, type and interpreter *)
module M : sig
  type exp = CONST of const
           | VAR of id
           | FN of id * exp
           | APP of exp * exp
           | LET of decl * exp
           | IF of exp * exp * exp
           | BOP of bop * exp * exp
           | READ
           | WRITE of exp
           | MALLOC of exp          (*   malloc e *)
           | ASSIGN of exp * exp    (*   e := e   *)
           | BANG of exp            (*   !e       *)
           | SEQ of exp * exp       (*   e ; e    *)
           | PAIR of exp * exp      (*   (e, e)   *)
           | FST of exp            (*   e.1      *)
           | SND of exp            (*   e.2      *)
  and const = S of string | N of int | B of bool
  and id = string
  and decl = 
    | REC of id * id * exp  (* Recursive function decl. (fun_id, arg_id, body) *)
    | VAL of id * exp       (* Value decl, including non-recursive functions *)
  and bop = ADD | SUB | EQ | AND | OR

  (* type in M  *)
  type typ = TyInt                      (* integer type *)
             | TyBool                   (* boolean type *)
             | TyString                 (* string type *)
             | TyPair of typ * typ      (* pair type *)
             | TyLoc of typ             (* location type *)

  (* errors *)
  exception RunError of string
  exception TypeError of string

  val run: exp -> unit
end =
struct
  type exp = CONST of const
           | VAR of id
           | FN of id * exp
           | APP of exp * exp
           | LET of decl * exp
           | IF of exp * exp * exp
           | BOP of bop * exp * exp
           | READ
           | WRITE of exp
           | MALLOC of exp          (*   malloc e *)
           | ASSIGN of exp * exp    (*   e := e   *)
           | BANG of exp            (*   !e       *)
           | SEQ of exp * exp       (*   e ; e    *)
           | PAIR of exp * exp      (*   (e, e)   *)
           | FST of exp            (*   e.1      *)
           | SND of exp            (*   e.2      *)
  and const = S of string | N of int | B of bool
  and id = string
  and decl = 
    | REC of id * id * exp  (* Recursive function decl. (fun_id, arg_id, body) *)
    | VAL of id * exp       (* Value decl, including non-recursive functions *)
  and bop = ADD | SUB | EQ | AND | OR

  (* type in M  *)
  type typ = TyInt                    (* integer type *)
             | TyBool                 (* boolean type *)
             | TyString               (* string type *)
             | TyPair of typ * typ    (* pair type *)
             | TyLoc of typ           (* location type *)

  (* errors *)
  exception RunError of string
  exception TypeError of string

  (* domains *)
  type loc = int
  type value = Int of int
             | String of string
             | Bool of bool
             | Loc of loc
             | Pair of value * value
             | Closure of closure
  and closure = fexpr * env
  and fexpr = Fun of id * exp
            | RecFun of id * id * exp
  and env = id -> value
  type memory = int * (loc -> value)

  (* notations (see 5 page in M.pdf) *)
  (* f @+ (x, v)              f[x |-> v]
   * store M (l, v)           M[l |-> v]
   * load M l                M(l)
   *)
  let loc_count = ref 0
  let (@+) f (x, v) = (fun y -> if y = x then v else f y)
  let store m (l, v) =  m @+ (l, v)
  let load m l = m l                
  let bind env (x, v) = env @+ (x, v)
  let malloc m = (loc_count := !loc_count + 1; (!loc_count, m))

  (* auxiliary functions *)
  let getInt = function 
    | (Int n) -> n 
    | _ -> raise (TypeError "not an int")

  let getString = function 
    | (String s) -> s 
    | _ -> raise (TypeError "not a string")

  let getBool = function 
    | (Bool b) -> b 
    | _ -> raise (TypeError "not a bool")

  let getLoc = function 
    | (Loc l) -> l 
    | _ -> raise (TypeError "not a loc")

  let getPair = function 
    | (Pair (a,b)) -> (a, b) 
    | _ -> raise (TypeError "not a pair")

  let getClosure = function 
    | (Closure c) -> c 
    | _ -> raise (TypeError "not a function")

  let op2fn =
    function ADD -> (fun (v1,v2) -> Int (getInt v1 + getInt v2))
    | SUB -> (fun (v1,v2) -> Int (getInt v1 - getInt v2))
    | AND -> (fun (v1,v2) -> Bool (getBool v1 && getBool v2))
    | OR ->  (fun (v1,v2) -> Bool (getBool v1 || getBool v2))
    | EQ ->
      (fun (v1, v2) -> 
        match (v1,v2) with
        | (Int n1, Int n2) -> Bool (n1 = n2)
        | (String s1, String s2) -> Bool (s1 = s2)
        | (Bool b1, Bool b2) -> Bool (b1 = b2)
        | (Loc l1, Loc l2) -> Bool (l1 = l2)
        | _ -> raise (TypeError "EQ operands are not int/bool/str/loc")
      )

  let rec printValue =
    function 
    | Int n -> print_endline (string_of_int n)
    | Bool b -> print_endline (string_of_bool b)
    | String s -> print_endline s
    | _ -> raise (TypeError "WRITE operand is not int/bool/string")

  let rec eval env mem exp = 
    match exp with
    | CONST (S s) -> (String s, mem)
    | CONST (N n) -> (Int n, mem)
    | CONST (B b) -> (Bool b, mem)
    | VAR x -> (env x, mem)
    | FN (x, e) -> (Closure (Fun (x, e), env), mem)
    | APP (e1, e2) ->
      let (v1, m') = eval env mem e1 in
      let (v2, m'') = eval env m' e2 in
      let (c, env') = getClosure v1 in
      (match c with 
      | Fun (x, e) -> eval (bind env' (x, v2)) m'' e
      | RecFun (f, x, e) ->
        let env'' = bind env' (x, v2) in
        let env''' = bind env'' (f, v1) in
        eval env''' m'' e)
    | IF (e1, e2, e3) ->
      let (v1, m') = eval env mem e1 in
      eval env m' (if getBool v1 then e2 else e3)
    | BOP (op, e1, e2) ->
      let (v1, m') = eval env mem e1 in
      let (v2, m'') = eval env m' e2 in
      ((op2fn op) (v1,v2), m'')
    | READ ->
      let n = try read_int () with _ -> raise (RunError "read error") in
      (Int n, mem)
    | WRITE e ->
      let (v, m') = eval env mem e in
      let _ = printValue v in
      (v, m')
    | PAIR (e1, e2) -> 
      let (v1, m') = eval env mem e1 in
      let (v2, m'') = eval env m' e2 in
      (Pair (v1, v2), m'')
    | FST e -> 
      let (v, m') = eval env mem e in
      (fst (getPair v), m')
    | SND e -> 
      let (v, m') = eval env mem e in
      (snd (getPair v), m')
    | SEQ (e1, e2) -> 
      let (v, m') = eval env mem e1 in
      eval env m' e2
    | LET (VAL (x, e1), e2) ->
      let (v1, m') = eval env mem e1 in
      eval (bind env (x,v1)) m' e2
    | LET (REC (f, x, e1), e2) -> 
      let closure = Closure (RecFun (f, x, e1), env) in
      eval (bind env (f, closure)) mem e2
    | MALLOC e ->
      let (v, m') = eval env mem e in
      let (l, m'') = malloc m' in  
      (Loc l, store m'' (l,v))
    | ASSIGN (e1, e2) -> 
      let (v1, m') = eval env mem e1 in
      let (v2, m'') = eval env m' e2 in
      (v2, store m'' (getLoc v1, v2))
    | BANG e -> 
      let (v, m') = eval env mem e in
      (load m' (getLoc v), m')

  let emptyEnv = (fun x -> raise (RunError ("unbound id: " ^ x)))

  let emptyMem = 
    (fun l -> raise (RunError ("uninitialized loc: " ^ string_of_int l)))

  let run exp = ignore (eval emptyEnv emptyMem exp)

end
