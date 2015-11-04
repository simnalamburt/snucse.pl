(*
 * SNU 4190.310 Programming Languages 2015 Fall
 * Treasure hunt
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = 
  | End of treasure
  | Branch of map * map
  | Guide of string * map

exception IMPOSSIBLE

type var = int

let var_id = ref 0 

let new_var () =
  let _ = var_id := !var_id + 1 in
  !var_id

type key' = BAR | VAR of var | NODE of (key' * key')

type equation = EQ of (key' * key') | AND of (equation * equation)

type link = (treasure * key') list

type substitution = (var * key') list

let rec build_equation : link -> map -> key' -> equation * link = fun lnk m k ->
  match m with
  | End StarBox -> (EQ (k, BAR), (StarBox, BAR) :: lnk)
  | End (NameBox x) ->
    let (k', lnk') = 
      try (List.assoc (NameBox x) lnk, lnk) with 
      Not_found -> let k' = VAR (new_var ()) in (k', (NameBox x, k') :: lnk)
    in
    (EQ (k, k'), lnk')
  | Guide (x, m1) ->
    let alpha, lnk' = 
      try (List.assoc (NameBox x) lnk, lnk) with 
      Not_found -> let k' = VAR (new_var ()) in (k', (NameBox x, k') :: lnk)
    in
    let beta = VAR (new_var ()) in
    let (eqn, lnk'') = build_equation lnk' m1 beta in
    (AND (eqn, EQ (k, NODE (alpha, beta))), lnk'')
  | Branch (m1, m2) ->
    let alpha = VAR (new_var ()) in
    let (eqn1, lnk') = build_equation lnk m1 (NODE (alpha, k)) in
    let (eqn2, lnk'') = build_equation lnk' m2 alpha in
    (AND (eqn1, eqn2), lnk'')

let rec substitute_key : key' -> substitution -> key' = fun k s ->
  match k with
  | BAR -> BAR
  | VAR v -> (try List.assoc v s with Not_found -> VAR v)
  | NODE (k1, k2) -> NODE (substitute_key k1 s, substitute_key k2 s)

let rec substitute_eqn : equation -> substitution -> equation = fun eqn s ->
  match eqn with
  | EQ (k1, k2) -> EQ (substitute_key k1 s, substitute_key k2 s)
  | AND (eqn1, eqn2) -> AND (substitute_eqn eqn1 s, substitute_eqn eqn2 s)
  
let rec does_contain k x = 
  match k with
  | BAR -> false
  | VAR x' -> x = x'
  | NODE (k1, k2) -> does_contain k1 x || does_contain k2 x

let rec compose_substitution s1 s2 =
  match s1 with
  | [] -> s2
  | (v, k) :: tail_s -> 
    let s2' = List.map (fun (v', k') -> (v', substitute_key k' [(v, k)])) s2 in
    compose_substitution tail_s ((v, k) :: s2')

let rec solve_equation eqn cur_subs =
  match eqn with 
  | EQ (BAR, BAR) -> cur_subs
  | EQ (VAR alpha, VAR beta) -> 
    if alpha = beta then cur_subs else 
      compose_substitution [alpha, VAR beta] cur_subs
  | EQ (VAR alpha, k) | EQ (k , VAR alpha) -> 
    if does_contain k alpha then raise IMPOSSIBLE else
      compose_substitution [(alpha, k)] cur_subs
  | EQ (NODE (k1, k2), NODE (k1', k2')) ->
    let subs = solve_equation (EQ (k1, k1')) [] in
    let k2 = substitute_key k2 subs in
    let k2' = substitute_key k2' subs in
    let subs' = solve_equation (EQ (k2, k2')) [] in
    compose_substitution (compose_substitution subs' subs) cur_subs
  | AND (eqn1, eqn2) -> 
    let new_subs = solve_equation eqn1 cur_subs in
    solve_equation (substitute_eqn eqn2 new_subs) new_subs
  | EQ (k1, k2) -> raise IMPOSSIBLE

let rec assign_key k s = 
  match k with
  | BAR -> Bar
  | VAR v -> (try assign_key (List.assoc v s) s with Not_found -> Bar)
  | NODE (k1, k2) -> Node (assign_key k1 s, assign_key k2 s)

let rec convert_to_key lnk subs = 
  match lnk with
  | [] -> []
  | (tr, k) :: tail_lnk -> 
    let cur_key = assign_key k subs in
    let tail_keys = convert_to_key tail_lnk subs in
    if List.mem cur_key tail_keys then tail_keys else cur_key :: tail_keys

let getReady : map -> key list = fun m ->
  let (eqn, lnk) = build_equation [] m (VAR (new_var ())) in
  let subs = solve_equation eqn [] in
  convert_to_key lnk subs
