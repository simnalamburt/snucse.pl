(*
 * SNU 4190.310 Programming Languages 
 * SM5 implementation skeleton code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

module type SM5 = 
sig
  type record
  type loc 
  type value = Z of int | B of bool | L of loc | Unit | R of record
  type cmd = 
    | PUSH of obj 
    | POP 
    | STORE 
    | LOAD 
    | JTR of command * command
    | MALLOC 
    | BOX of int 
    | UNBOX of string 
    | BIND of string 
    | UNBIND
    | GET 
    | PUT 
    | CALL 
    | ADD 
    | SUB 
    | MUL 
    | DIV
    | EQ
    | LESS
    | NOT
  and obj = Val of value | Id of string | Fn of string * command
  and command = cmd list

  exception GC_Failure
  exception Error of string

  val gc_mode : bool ref
  val run : command -> unit

end

module Sm5 : SM5 =
struct
  type loc = int * int
  type record = (string * loc) list
  type value = Z of int | B of bool | L of loc | Unit | R of record
  type cmd = 
    | PUSH of obj 
    | POP 
    | STORE 
    | LOAD 
    | JTR of command * command
    | MALLOC 
    | BOX of int 
    | UNBOX of string 
    | BIND of string 
    | UNBIND
    | GET 
    | PUT 
    | CALL 
    | ADD 
    | SUB 
    | MUL 
    | DIV 
    | EQ 
    | LESS 
    | NOT
  and obj = Val of value | Id of string | Fn of string * command
  and command = cmd list

  type proc = string * command * environment
  and evalue = Loc of loc | Proc of proc
  and environment = (string * evalue) list

  type svalue = V of value | P of proc | M of (string * evalue)
  type stack = svalue list
  type memory = (loc * value) list
  type continuation = (command * environment) list

  exception GC_Failure
  exception Error of string

  let gc_mode = ref false

  let loc_to_str (base, offset) = 
    Printf.sprintf "<%d, %d>" base offset

  let val_to_str = function 
    | Z i -> string_of_int i
    | B b -> string_of_bool b
    | R pair_list -> 
      let str_list = 
        List.map (fun (s, l) -> Printf.sprintf "(%s, %s)" s (loc_to_str l)) pair_list
      in
      "{" ^  (String.concat ", " str_list) ^ "}"
    | Unit -> "()"
    | L l -> loc_to_str l

  let rec cmd_to_str = function
    | PUSH o -> Printf.sprintf "push %s" (obj_to_str o)
    | POP -> "pop"
    | STORE -> "store"
    | LOAD -> "load"
    | JTR (c1, c2) -> 
      Printf.sprintf "jtr (%s, %s)" (command_to_str c1) (command_to_str c2)
    | MALLOC -> "malloc"
    | BOX z -> Printf.sprintf "box %d" z
    | UNBOX x -> Printf.sprintf "unbox %s" x
    | BIND x -> Printf.sprintf "bind %s" x
    | UNBIND -> "unbind"
    | GET -> "get"
    | PUT -> "put"
    | CALL -> "call"
    | ADD -> "add"
    | SUB -> "sub"
    | MUL -> "mul"
    | DIV -> "div"
    | EQ -> "eq"
    | LESS -> "less"
    | NOT -> "not"
  and obj_to_str = function
    | Val v -> val_to_str v
    | Id x -> "\"" ^ x ^ "\""
    | Fn (f, comm) -> Printf.sprintf "%s { %s }" f (command_to_str comm)
  and command_to_str comm = 
    "[" ^ (String.concat "; " (List.map cmd_to_str comm)) ^ "]"
  
  let rec proc_to_str (f, comm, env) = 
    Printf.sprintf "%s {%s}, env : %s" f (command_to_str comm) (env_to_str env)
  and evalue_to_str = function
    | Loc l -> loc_to_str l
    | Proc p -> proc_to_str p
  and env_to_str env =
    let entry_str_list = 
      List.map (fun (x, ev) -> Printf.sprintf "%s : %s" x (evalue_to_str ev)) env 
    in
    "[" ^ (String.concat ";\n" entry_str_list) ^ "]"

  let sval_to_str = function
    | V v -> val_to_str v
    | P p -> proc_to_str p
    | M (x, ev) -> Printf.sprintf "(%s, %s)" x (evalue_to_str ev)

  let stack_to_str s = 
    "[" ^ (String.concat ";\n" (List.map sval_to_str s)) ^ "]"

  let mem_to_str m = 
    let entry_to_str l v = 
      Printf.sprintf "%s : %s\n" (loc_to_str l) (val_to_str v) 
    in
    List.fold_left (fun acc_str (l, v) -> acc_str ^ entry_to_str l v) "" m

  let lookup_env x e =
    try List.assoc x e with Not_found -> raise (Error ("Unbound name : " ^ x))

  let lookup_record x r = 
    try List.assoc x r with Not_found -> raise (Error ("Unfound field : " ^ x))

  let load l m = 
    try List.assoc l m with 
    Not_found -> raise (Error ("Uninitialized location : %s" ^ (loc_to_str l)))

  let store l v m = 
    if List.mem_assoc l m then 
      (l, v) :: (List.remove_assoc l m) 
    else (l, v) :: m

  let mem_limit = 128

  let loc_id = ref 0

  let reachable_locs : (loc list) ref = ref []

  (* TODO : complete this function.
   * Implement the GC algorithm introduced in class material.
   *)
  let malloc_with_gc s m e c k =
    if List.length m < mem_limit then
      let _ = loc_id := !loc_id + 1 in
      ((!loc_id, 0), m)
    else
      let _ = reachable_locs := [] in
      (* TODO : Add the code that marks the reachable locations.
       * let _ = ... 
       *)
      let new_m = List.filter (fun (l, _) -> List.mem l !reachable_locs) m in
      if List.length new_m < mem_limit then
        let _ = loc_id := !loc_id + 1 in
        let new_l = (!loc_id, 0) in
        (new_l, new_m)
      else
        raise GC_Failure

  let malloc () = 
    let _ = loc_id := !loc_id + 1 in
    (!loc_id, 0)

  let rec box_stack s i accum_list =
    if i = 0 then (V (R accum_list)) :: s else
      match s with
      | M (x, Loc l) :: s' -> box_stack s' (i - 1) ((x, l) :: accum_list)
      | _ -> raise (Error "Box() : entry is not (x, loc) pair")

  let is_equal v1 v2 =
    match v1, v2 with
    | Z z1, Z z2 -> z1 = z2
    | B b1, B b2 -> b1 = b2
    | R r1, R r2 -> 
      List.sort Pervasives.compare r1 = List.sort Pervasives.compare r2
    | Unit, Unit -> true
    | L (base1, z1), L (base2, z2) ->
      base1 = base2 && z1 = z2
    | _ , _ -> false

  let rec step = function
    | (s, m, e, PUSH (Val v) :: c, k) -> (V v :: s, m, e, c, k)
    | (s, m, e, PUSH (Id x) :: c, k) ->
      (match lookup_env x e with
      | Loc l -> (V (L l) :: s, m, e, c, k)
      | Proc p -> (P p :: s, m, e, c, k))
    | (s, m, e, PUSH (Fn (x, c')) :: c, k) -> (P (x, c',e)::s, m, e, c, k)
    | (w :: s, m, e, POP :: c , k) -> (s, m, e, c, k)
    | (V (L l) :: V v :: s, m, e, STORE :: c, k) -> (s, store l v m, e, c, k)
    | (V (L l) :: s, m, e, LOAD :: c, k) -> (V (load l m) :: s, m, e, c, k)
    | (V (B b)::s, m, e, JTR (c1,c2) :: c, k) -> 
      (s, m, e, (if b then c1 @ c else (c2 @ c)), k)
    | (s, m, e, MALLOC :: c, k) -> 
      if !gc_mode then
        let (new_l, new_m) = malloc_with_gc s m e c k in
        (V (L new_l) :: s, new_m, e, c, k)
      else 
        (V (L (malloc ())) :: s, m, e, c, k)
    | (s, m, e, BOX z :: c, k) ->
      if z <= 0 then 
        raise (Error "Box(n): non-positive n") 
      else 
        (box_stack s z [], m, e, c, k)
    | (V (R r) :: s, m, e, UNBOX x :: c, k) -> 
      (V (L (lookup_record x r)) :: s, m, e, c, k)
    | (V (L l) :: s, m, e, BIND x :: c, k) -> (s, m, (x, Loc l) :: e, c, k)
    | (P p :: s, m, e, BIND x :: c, k) -> (s, m, (x, Proc p) :: e, c, k)
    | (s, m, (x, ev) :: e, UNBIND :: c, k) -> (M (x, ev) :: s, m, e, c, k)
    | (V (L l) :: V v :: P (x, c', e') :: s, m, e, CALL :: c, k) ->
      (s, store l v m, (x, Loc l) :: e', c', (c, e) :: k)
    | (s, m, e, [], (c, e') :: k) -> (s, m, e', c, k)
    | (s, m, e, GET :: c, k) -> (V (Z (read_int())) :: s, m, e, c, k)
    | (V (Z z) :: s, m, e, PUT :: c, k) -> 
      let _ = print_endline (string_of_int z) in
      (s, m, e, c, k)
    | (V (Z z2) :: V (Z z1) :: s, m, e, ADD :: c, k) ->
      (V (Z (z1 + z2))::s, m, e, c, k)
    | (V (Z z) :: V (L (base, offset)) :: s, m, e, ADD :: c, k)
    | (V (L (base, offset)) :: V (Z z) :: s, m, e, ADD :: c, k) -> 
      if offset + z >= 0 then
        (V (L (base, offset + z)) :: s, m, e, c, k) 
      else 
        raise (Error "Negative loc offset spawned")
    | (V (Z z2) :: V (Z z1) :: s, m, e, SUB :: c, k) ->
      (V (Z (z1 - z2)) :: s, m, e, c, k)
    | (V (Z z) :: V (L (base, offset)) :: s, m, e, SUB :: c, k) -> 
      if offset - z >= 0 then
        (V (L (base, offset - z)) :: s, m, e, c, k) 
      else  
        raise (Error "Negative loc offset spanwed")
    | (V (Z z2) :: V (Z z1) :: s, m, e, MUL :: c, k) -> 
      (V (Z (z1 * z2)) :: s, m, e, c, k)
    | (V (Z z2) :: V (Z z1) :: s, m, e, DIV :: c, k) -> 
      (V (Z (z1 / z2)) :: s, m, e, c, k)
    | (V v2 :: V v1 :: s, m, e, EQ :: c, k) -> 
      (V (B (is_equal v1 v2)) :: s, m, e, c, k)
    | (V (Z z2) :: V (Z z1) :: s, m, e, LESS :: c, k) -> 
      (V (B (z1 < z2)) :: s, m, e, c, k)
    | (V (B b) :: s, m, e, NOT :: c, k) -> (V (B (not b)) :: s, m, e, c, k)
    | s, m, e, c, k ->
      (* For debugging, uncomment the following lines.
      let _ = print_endline "***** Command *****" in
      let _ = print_endline (command_to_str c) in
      let _ = print_endline "***** Stack *****" in
      let _ = print_endline (stack_to_str s) in
      let _ = print_endline "***** Environment *****" in
      let _ = print_endline (env_to_str e) in
      let _ = print_endline "***** Memory *****" in
      let _ = print_endline (mem_to_str m) in
      *)
      raise (Error "Invalid machine state")

  let rec run_helper (s, m, e, c, k) = 
    match c, k with
    | [], [] -> ()
    | _ -> run_helper (step (s, m, e, c, k))

  let run c = run_helper ([], [], [], c, []) 

end
