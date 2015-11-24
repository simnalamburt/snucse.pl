(*
 * SNU 4190.310 Programming Languages 2015 Fall
 * M Interpreter Skeleton Code
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

  (* types in M  *)
  type types = TyInt                     (* integer type *)
             | TyBool                    (* boolean type *)
             | TyString                  (* string type *)
             | TyPair of types * types   (* pair type *)
             | TyLoc of types            (* location type *)
             | TyArrow of types * types  (* function type *)

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

  (* types in M  *)
  type types = TyInt                     (* integer type *)
             | TyBool                    (* boolean type *)
             | TyString                  (* string type *)
             | TyPair of types * types   (* pair type *)
             | TyLoc of types            (* location type *)
             | TyArrow of types * types  (* function type *)

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
  let (@+) f (x, v) = (fun y -> if y = x then v else f y)
  let store ((l, m): memory) (p: loc * value): memory = (l, m @+ p)
  let load ((_, m): memory) (l: loc): value = m l
  let malloc ((l, m): memory) = (l, (l+1, m))

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

  let getLoc: value -> loc = function
    | (Loc l) -> l
    | _ -> raise (TypeError "not a loc")

  let getPair = function
    | (Pair (a,b)) -> (a, b)
    | _ -> raise (TypeError "not a pair")

  let getClosure = function
    | (Closure c) -> c
    | _ -> raise (TypeError "not a function")

  let op2fn (operator: bop) ((v1, v2): value * value): value =
    match operator with
    | ADD -> Int (getInt v1 + getInt v2)
    | SUB -> Int (getInt v1 - getInt v2)
    | AND -> Bool (getBool v1 && getBool v2)
    | OR -> Bool (getBool v1 || getBool v2)
    | EQ -> begin
      match v1, v2 with
      | Int left, Int right       -> Bool (left = right)
      | String left, String right -> Bool (left = right)
      | Bool left, Bool right     -> Bool (left = right)
      | Loc left, Loc right       -> Bool (left = right)
      | _ -> raise (TypeError "ㅇㅅㅇ")
    end

  let rec printValue =
    function
    | Int n -> print_endline (string_of_int n)
    | Bool b -> print_endline (string_of_bool b)
    | String s -> print_endline s
    | _ -> raise (TypeError "WRITE operand is not int/bool/string")

  let rec eval (env: env) (mem: memory) (exp: exp): (value * memory) =
    match exp with
    | CONST (S s) -> (String s, mem)
    | CONST (N n) -> (Int n, mem)
    | CONST (B b) -> (Bool b, mem)
    | VAR x -> (env x, mem)
    | FN (x, e) -> (Closure (Fun (x, e), env), mem)
    | APP (efunc, eparam) -> begin
      let vfunc, mem = eval env mem efunc in
      let vparam, mem = eval env mem eparam in
      let closure, env = getClosure vfunc in
      match closure with
      | Fun (iarg, ebody) -> begin
        let env = env @+ (iarg, vparam) in
        eval env mem ebody
      end
      | RecFun (ifun, iarg, ebody) -> begin
        let env = env @+ (iarg, vparam) in
        let env = env @+ (ifun, vfunc) in
        eval env mem ebody
      end
    end
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
    | LET (REC (ifun, iarg, ebody), exp) -> begin
      let func = RecFun (ifun, iarg, ebody) in
      let closure = Closure (func, env) in
      let env = env @+ (ifun, closure) in
      eval env mem exp
    end
    | LET (VAL (iname, evalue), exp) -> begin
      let value, mem = eval env mem evalue in
      let env = env @+ (iname, value) in
      eval env mem exp
    end
    | MALLOC exp -> begin
      let value, mem = eval env mem exp in
      let loc, mem = malloc mem in
      let mem = store mem (loc, value) in
      (Loc loc, mem)
    end
    | ASSIGN (eleft, eright) -> begin
      let ret, mem = eval env mem eleft in
      let location = getLoc ret in
      let value, mem = eval env mem eright in
      let mem = store mem (location, value) in
      (value, mem)
    end
    | BANG exp -> begin
      let ret, mem = eval env mem exp in
      let location = getLoc ret in
      let value = load mem location in
      (value, mem)
    end
    | SEQ (ebefore, eafter) -> begin
      let _, mem = eval env mem ebefore in
      eval env mem eafter
    end

  let emptyEnv = (fun x -> raise (RunError ("unbound id: " ^ x)))

  let emptyMem =
    (0, fun l -> raise (RunError ("uninitialized loc: " ^ string_of_int l)))

  let run (exp: exp) = ignore (eval emptyEnv emptyMem exp)

end

module type M_TypeChecker = sig
    val check: M.exp -> M.types
end
