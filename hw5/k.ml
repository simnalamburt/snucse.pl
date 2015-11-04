(*
 * SNU 4190.310 Programming Languages 2015 Fall
 *  K-- Interpreter 
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | ASSIGN of id * exp          (* assgin to variable *)
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | FOR of id * exp * exp * exp (* for loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id * exp * exp (* procedure binding *)
  | CALLV of id * exp           (* call by value *)
  | CALLR of id * id            (* call by referenece *)
  | READ of id
  | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value
  val emptyMemory : memory
  val emptyEnv : env
  val run : program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string

  type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | ASSIGN of id * exp          (* assgin to variable *)
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | FOR of id * exp * exp * exp (* for loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id * exp * exp (* procedure binding *)
  | CALLV of id * exp           (* call by value *)
  | CALLR of id * id            (* call by referenece *)
  | READ of id
  | WRITE of exp
  type program = exp

  type value =
  | Num of int
  | Bool of bool
  | Unit
    
  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
      match v with
      | Unit -> ()
      | _ -> raise (Error "TypeError : not unit")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id, exp, env) -> (id, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval : memory -> env -> exp -> (value * memory) = fun mem env e ->
    match e with
    | UNIT -> (Unit, mem)
    | NUM i -> (Num i, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | VAR id -> (Mem.load mem (lookup_env_loc env id), mem)
    | ADD (e1, e2) -> 
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      (Num (value_int v1 + value_int v2), mem'')
    | SUB (e1, e2) -> 
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      (Num (value_int v1 - value_int v2), mem'')
    | MUL (e1, e2) -> 
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      (Num (value_int v1 * value_int v2), mem'')
    | DIV (e1, e2) -> 
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      (Num (value_int v1 / value_int v2), mem'')
    | EQUAL (e1, e2) ->
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      let b = 
        match (v1, v2) with
        | (Num n1, Num n2) -> n1 = n2
        | (Bool b1, Bool b2) -> b1 = b2
        | (Unit, Unit) -> true
        | _ -> false
      in
      (Bool b, mem'')
    | LESS (e1, e2) ->
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      (Bool (value_int v1 < value_int v2), mem'')
    | NOT e1 ->
      let v, mem' = eval mem env e1 in
      (Bool (not (value_bool v)), mem')
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | LETF (f, x, e1, e2) ->
      eval mem (Env.bind env f (Proc (x, e1, env))) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | IF (e_cond, e_true, e_false) ->
      let (v_cond, mem') = eval mem env e_cond in
      if value_bool v_cond then eval mem' env e_true else eval mem' env e_false
    | WHILE (e_cond, e_body) -> 
      let (v_cond, mem') = eval mem env e_cond in
      if value_bool v_cond then 
        let ( _ , mem'') = eval mem env e_body in
        eval mem'' env (WHILE (e_cond, e_body))
      else
        (Unit, mem')
    | FOR (id, e1, e2, e_body) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      evalfor id n1 n2 mem'' env e_body
    | SEQ (e1, e2) ->
      let ( _ , mem') = eval mem env e1 in
      eval mem' env e2
    | CALLV (f, arg_exp) ->
      let (arg_v, mem') = eval mem env arg_exp in
      let (arg_id, e_body, env_saved) = lookup_env_proc env f in
      let (l, mem'') = Mem.alloc mem' in
      let env' = Env.bind env_saved arg_id (Addr l) in
      let new_mem = Mem.store mem'' l arg_v in
      (* Now, bind procedure 'f', to support recursion *)
      let env'' = Env.bind env' f (Proc (arg_id, e_body, env_saved)) in
      eval new_mem env'' e_body
    | CALLR (f, arg_var) ->
      let (arg_id, e_body, env_saved) = lookup_env_proc env f in
      let ref_entry = Addr (lookup_env_loc env arg_var) in
      let env' = Env.bind env_saved arg_id ref_entry in
      (* Now, bind procedure 'f', to support recursion *)
      let env'' = Env.bind env' f (Proc (arg_id, e_body, env_saved)) in
      eval mem env'' e_body

    (* FOR evaluation helper function *)
    and evalfor id n1 n2 mem env e = 
      if n1 > n2 then
        (Unit, mem) 
      else
        let loc = lookup_env_loc env id in
        let mem' = Mem.store mem loc (Num n1) in
        let ( _ , mem'') = eval mem' env e in
        evalfor id (n1 + 1) n2 mem'' env e

  let run pgm = 
    let (v, _ ) = eval emptyMemory emptyEnv pgm in
    v
end
