(* Location Signature *)
module type LOC = sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC = struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV = sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM = struct
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
module Env : ENV = struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS = sig
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
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list      (* call by value *)
  | CALLR of id * id list       (* call by referenece *)
  | RECORD of (id * exp) list   (* record construction *)
  | FIELD of exp * id           (* access record field *)
  | ASSIGN of id * exp          (* assgin to variable *)
  | ASSIGNF of exp * id * exp   (* assign to record field *)
  | READ of id
  | WRITE of exp

  type program = exp
  type memory
  type env
  type value =
  | Num of int
  | Bool of bool
  | Unit
  | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS = struct
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
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list      (* call by value *)
  | CALLR of id * id list       (* call by referenece *)
  | RECORD of (id * exp) list   (* record construction *)
  | FIELD of exp * id           (* access record field *)
  | ASSIGN of id * exp          (* assgin to variable *)
  | ASSIGNF of exp * id * exp   (* assign to record field *)
  | READ of id
  | WRITE of exp

  type program = exp

  type value =
  | Num of int
  | Bool of bool
  | Unit
  | Record of (id -> Loc.t)

  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

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

  let value_record v =
    match v with
      | Record r -> r
      | _ -> raise (Error "TypeError : not record")

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
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval (mem: memory) (env: env) (einput: exp): value * memory =
    let calc_int (mem: memory) (env: env) (e1: exp) (e2: exp) (f: int -> int -> int): value * memory =
      let v1, mem = eval mem env e1 in
      let v2, mem = eval mem env e2 in
      let result = Num (f (value_int v1) (value_int v2)) in
      result, mem
    in
    let calc_bool (mem: memory) (env: env) (e1: exp) (e2: exp) (f: int -> int -> bool): value * memory =
      let v1, mem = eval mem env e1 in
      let v2, mem = eval mem env e2 in
      let result = Bool (f (value_int v1) (value_int v2)) in
      result, mem
    in
    match einput with
    | READ name -> begin
      let value = Num (read_int()) in
      let loc = lookup_env_loc env name in
      let mem = Mem.store mem loc value in
      value, mem
    end
    | WRITE exp -> begin
      let value, mem = eval mem env exp in
      print_endline (string_of_int (value_int value));
      value, mem
    end
    | LETV (name, exp, enext) -> begin
      let value, mem = eval mem env exp in
      let loc, mem = Mem.alloc mem in
      let mem = Mem.store mem loc value in
      let env = Env.bind env name (Addr loc) in
      eval mem env enext
    end
    | ASSIGN (name, e) -> begin
      let value, mem = eval mem env e in
      let loc = lookup_env_loc env name in
      let mem = Mem.store mem loc value in
      value, mem
    end
    | NUM value -> Num value, mem
    | TRUE      -> Bool true, mem
    | FALSE     -> Bool false, mem
    | UNIT      -> Unit, mem
    | VAR name -> Mem.load mem (lookup_env_loc env name), mem
    | ADD   (eleft, eright) -> calc_int  mem env eleft eright ( + )
    | SUB   (eleft, eright) -> calc_int  mem env eleft eright ( - )
    | MUL   (eleft, eright) -> calc_int  mem env eleft eright ( * )
    | DIV   (eleft, eright) -> calc_int  mem env eleft eright ( / )
    | EQUAL (eleft, eright) -> calc_bool mem env eleft eright ( = )
    | LESS  (eleft, eright) -> calc_bool mem env eleft eright ( < )
    | NOT exp -> begin
      let value, mem = eval mem env exp in
      Bool (not (value_bool value)), mem
    end
    | SEQ (exp, enext) -> begin
      let _, mem = eval mem env exp in
      eval mem env enext
    end
    | IF (econd, ethen, eotherwise) -> begin
      let cond, mem = eval mem env econd in
      let enext = if value_bool cond then ethen else eotherwise in
      eval mem env enext
    end
    | WHILE (econd, ebody) -> begin
      let cond, mem = eval mem env econd in
      if value_bool cond then
        let _, mem = eval mem env ebody in
        eval mem env einput
      else
        Unit, mem
    end
    | LETF (name, params, eimpl, enext) -> begin
      let entry = Proc (params, eimpl, env) in
      let env = Env.bind env name entry in
      eval mem env enext
    end
    | CALLV (name, eparams) -> begin
      let batch_eval ((values, mem): value list * memory) (exp: exp): value list * memory =
        let value, mem = eval mem env exp in
        values@[value], mem
      in
      let values, mem = List.fold_left batch_eval ([], mem) eparams in
      Unit, mem (* TODO *)
    end
    | _ -> failwith "Unimplemented" (* TODO : Implement rest of the cases *)

  let run (mem, env, pgm) =
    let (v, _ ) = eval mem env pgm in
    v
end
