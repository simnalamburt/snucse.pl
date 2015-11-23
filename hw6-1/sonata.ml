(*
 * SNU 4190.310 Programming Languages 
 * Sonata Implementation
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

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
type memory = (loc * svalue) list

exception Error of string

let debug_mode = ref false

let loc_to_str (base, offset) = 
  Printf.sprintf "<%d, %d>" base offset

let val_to_str = function 
  | Z i -> string_of_int i
  | B b -> string_of_bool b
  | R pair_list -> 
    let pair_to_str (s, l) = Printf.sprintf "(%s, %s)" s (loc_to_str l) in
    "{" ^  (String.concat ", " (List.map pair_to_str pair_list)) ^ "}"
  | Unit -> "unit"
  | L l -> loc_to_str l

let rec cmd_to_str indent cmd = 
  match cmd with
  | PUSH obj -> Printf.sprintf "push %s" (obj_to_str indent obj)
  | POP -> "pop"
  | STORE -> "store"
  | LOAD -> "load"
  | JTR (c1, c2) -> 
    let c1_str = (command_to_str ("  " ^ indent) c1) in
    let c2_str = (command_to_str ("  " ^ indent) c2) in
    Printf.sprintf "jtr (\n%s,\n%s)" c1_str c2_str
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
and obj_to_str indent obj = 
  match obj with
  | Val v -> val_to_str v
  | Id x -> x
  | Fn (f, comm) -> 
    let new_indent = "  " ^ indent in
    let comm_str = command_to_str new_indent comm in
    Printf.sprintf "(%s,\n%s\n%s)" f comm_str new_indent
and command_to_str indent comm = 
  indent ^ "[" ^ 
  (String.concat (";\n" ^ indent) (List.map (cmd_to_str indent) comm)) ^ "]"

let rec proc_to_str indent (x, comm, env) =
  let new_indent = ("  " ^ indent) in
  let comm_str = command_to_str new_indent comm in
  let env_str = env_to_str new_indent env in
  Printf.sprintf "(fun %s ->\n%s\n%s), Env =\n%s" x comm_str new_indent env_str
and evalue_to_str indent ev =
  match ev with
  | Loc l -> loc_to_str l
  | Proc p -> proc_to_str indent p
and env_to_str indent env =
  let entry_to_str (x, ev) = 
    Printf.sprintf "%s : %s" x (evalue_to_str indent ev)
  in
  indent ^ "[" ^ 
  (String.concat (";\n" ^ indent) (List.map entry_to_str env)) ^ "]"

let sval_to_str = function
  | V v -> val_to_str v
  | P p -> proc_to_str "" p
  | M (x, ev) -> Printf.sprintf "(%s, %s)" x (evalue_to_str "" ev)

let stack_to_str s =
  "[" ^ (String.concat ";\n" (List.map sval_to_str s)) ^ "]"

let mem_to_str m =
  let entry_to_str (l, v) =
    Printf.sprintf "%s : %s" (loc_to_str l) (sval_to_str v)
  in
  String.concat "\n" (List.map entry_to_str m)

let lookup_env x e =
  try List.assoc x e with Not_found -> raise (Error ("Unbound name : " ^ x))

let lookup_record x r =
  try List.assoc x r with Not_found -> raise (Error ("Unfound field : " ^ x))

let load l m =
  try List.assoc l m with
  Not_found -> raise (Error ("Uninitialized location : %s" ^ (loc_to_str l)))

let store l sv m =
  if List.mem_assoc l m then
    (l, sv) :: (List.remove_assoc l m)
  else (l, sv) :: m

let loc_id = ref 0

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
  | (s, m, e, PUSH (Val v) :: c) -> (V v :: s, m, e, c)
  | (s, m, e, PUSH (Id x) :: c) ->
    (match lookup_env x e with
    | Loc l -> (V (L l) :: s, m, e, c)
    | Proc p -> (P p :: s, m, e, c))
  | (s, m, e, PUSH (Fn (x, c')) :: c) -> (P (x, c',e)::s, m, e, c)
  | (w :: s, m, e, POP :: c) -> (s, m, e, c)
  | (V (L l) :: sv :: s, m, e, STORE :: c) -> (s, store l sv m, e, c)
  | (V (L l) :: s, m, e, LOAD :: c) -> (load l m :: s, m, e, c)
  | (V (B b)::s, m, e, JTR (c1,c2) :: c) -> 
    (s, m, e, (if b then c1 @ c else (c2 @ c)))
  | (s, m, e, MALLOC :: c) -> (V (L (malloc ())) :: s, m, e, c)
  | (s, m, e, BOX z :: c) ->
    if z <= 0 then 
      raise (Error "Box(n): non-positive n") 
    else 
      (box_stack s z [], m, e, c)
  | (V (R r) :: s, m, e, UNBOX x :: c) -> 
    (V (L (lookup_record x r)) :: s, m, e, c)
  | (V (L l) :: s, m, e, BIND x :: c) -> (s, m, (x, Loc l) :: e, c)
  | (P p :: s, m, e, BIND x :: c) -> (s, m, (x, Proc p) :: e, c)
  | (s, m, (x, ev) :: e, UNBIND :: c) -> (M (x, ev) :: s, m, e, c)
  | (V (L l) :: sv :: P (x, c', e') :: s, m, e, CALL :: c) ->
    (s, store l sv m, (x, Loc l) :: e', c')
  | (s, m, e, GET :: c) -> (V (Z (read_int())) :: s, m, e, c)
  | (V (Z z) :: s, m, e, PUT :: c) -> 
    let _ = print_endline (string_of_int z) in
    (s, m, e, c)
  | (V (Z z2) :: V (Z z1) :: s, m, e, ADD :: c) ->
    (V (Z (z1 + z2))::s, m, e, c)
  | (V (Z z) :: V (L (base, offset)) :: s, m, e, ADD :: c)
  | (V (L (base, offset)) :: V (Z z) :: s, m, e, ADD :: c) -> 
    if offset + z >= 0 then
      (V (L (base, offset + z)) :: s, m, e, c) 
    else 
      raise (Error "Negative loc offset spawned")
  | (V (Z z2) :: V (Z z1) :: s, m, e, SUB :: c) ->
    (V (Z (z1 - z2)) :: s, m, e, c)
  | (V (Z z) :: V (L (base, offset)) :: s, m, e, SUB :: c) -> 
    if offset - z >= 0 then
      (V (L (base, offset - z)) :: s, m, e, c) 
    else  
      raise (Error "Negative loc offset spanwed")
  | (V (Z z2) :: V (Z z1) :: s, m, e, MUL :: c) -> 
    (V (Z (z1 * z2)) :: s, m, e, c)
  | (V (Z z2) :: V (Z z1) :: s, m, e, DIV :: c) -> 
    (V (Z (z1 / z2)) :: s, m, e, c)
  | (V v2 :: V v1 :: s, m, e, EQ :: c) -> 
    (V (B (is_equal v1 v2)) :: s, m, e, c)
  | (V (Z z2) :: V (Z z1) :: s, m, e, LESS :: c) -> 
    (V (B (z1 < z2)) :: s, m, e, c)
  | (V (B b) :: s, m, e, NOT :: c) -> (V (B (not b)) :: s, m, e, c)
  | s, m, e, c ->
    raise (Error "Invalid machine state")

let rec run_helper (s, m, e, c) = 
  match c with
  | [] -> ()
  | _ -> 
    let _ = if !debug_mode then
      let _ = print_endline "====== Machine state ======" in
      let _ = print_newline() in
      let _ = print_endline "***** Command *****" in
      let _ = print_endline (command_to_str "" c) in
      let _ = print_newline() in
      let _ = print_endline "***** Stack *****" in
      let _ = print_endline (stack_to_str s) in
      let _ = print_newline() in
      let _ = print_endline "***** Environment *****" in
      let _ = print_endline (env_to_str "" e) in
      let _ = print_newline() in
      let _ = print_endline "***** Memory *****" in
      let _ = print_endline (mem_to_str m) in
      print_newline()
    in
    run_helper (step (s, m, e, c))

let run c = run_helper ([], [], [], c) 
