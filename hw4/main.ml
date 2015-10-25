(*
 * Spec
 *)
(* Term *)
type treasure = StarBox (* constant *)
              | NameBox of string (* variable *)

(* Type *)
type key = Bar (* arbitrary type *)
         | Node of key * key (* function *)

(* Expression *)
type map = End of treasure (* term *)
         | Branch of map * map (* function call *)
         | Guide of string * map (* function definition *)

exception IMPOSSIBLE


(*
 * Implementation
 *)
type id = string

type term = Number
          | Variable of id

type expression = Term of term
                | FnCall of expression * expression
                | FnDef of id * expression

type tyvar = id
type ty = TyVar of tyvar
        | Constant
        | Function of ty * ty
type tyenv = (id * ty) list

type substitution = (tyvar * ty) list

let rec apply (subst: substitution) (ty: ty): ty =
  match ty with
  | TyVar right when List.mem_assoc right subst -> List.assoc right subst
  | Function(_, ret) -> apply subst ret
  | _ -> ty

let apply_env (subst: substitution) (tyenv: tyenv): tyenv =
  let apply_entity ((name, ty): (id * ty)): (id * ty) =
    (name, apply subst ty)
  in
  List.map apply_entity tyenv


let rec occurs (var: tyvar) (ty: ty): bool =
  match ty with
  | TyVar right -> var = right
  | Constant -> false
  | Function(tparam, tret) -> occurs var tparam || occurs var tret

let rec unify (left: ty) (right: ty): substitution =
  match left, right with
  | left, right when left = right -> []
  | TyVar alpha, ty
  | ty, TyVar alpha when not (occurs alpha ty) -> [(alpha, ty)]
  | Function(tleft1, tleft2), Function(tright1, tright2) -> begin
    let subst1 = unify tleft1 tright1 in
    let subst2 = unify (apply subst1 tleft2) (apply subst1 tright2) in
    subst2 @ subst1 (* Note: tyvar 중복체크 안해도 됨 *)
  end
  | _ -> raise IMPOSSIBLE


(*
 * The *M* Algorithm
 *
 * LEE, Oukseh; YI, Kwangkeun. Proofs about a folklore let-polymorphic type
 * inference algorithm. ACM Transactions on Programming Languages and Systems
 * (TOPLAS), 1998, 20.4: 707-723.
 *)
let new_variable: ty =
  (* TODO *)
  TyVar ""

let rec m_algorithm (tyenv: tyenv) (exp: expression) (ty: ty): substitution =
  match exp with
  | Term Number -> unify Constant ty
  | Term Variable x when List.mem_assoc x tyenv -> begin
    let tright = List.assoc x tyenv in
    unify ty tright
  end
  | FnDef(name, edef) -> begin
    let alpha1 = new_variable in
    let alpha2 = new_variable in
    let subst1 = unify (Function(alpha1, alpha2)) ty in
    let subst2 = m_algorithm (apply_env subst1 tyenv @ [(name, apply subst1 alpha1)]) edef (apply subst1 alpha2) in
    subst2 @ subst1 (* Note: tyvar 중복체크 안해도 됨 *)
  end
  | FnCall(efunc, eparam) -> begin
    let alpha = new_variable in
    let subst1 = m_algorithm tyenv efunc (Function(alpha, ty)) in
    let subst2 = m_algorithm (apply_env subst1 tyenv) eparam (apply subst1 alpha) in
    subst2 @ subst1 (* Note: tyvar 중복체크 안해도 됨 *)
  end
  | _ -> raise IMPOSSIBLE
