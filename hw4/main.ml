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
  | TyVar(right) -> List.assoc right subst
  | Constant -> Constant
  | Function(_, ret) -> apply subst ret

let rec occurs (var: tyvar) (ty: ty): bool =
  match ty with
  | TyVar(right) -> var = right
  | Constant -> false
  | Function(tparam, tret) -> occurs var tparam || occurs var tret

let rec unify (left: ty) (right: ty): substitution =
  match left, right with
  | left, right when left = right -> []
  | TyVar(alpha), ty
  | ty, TyVar(alpha) when not (occurs alpha ty) -> [(alpha, ty)]
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
let m_algorithm (tyenv: tyenv) (exp: expression) (ty: ty): substitution =
  (* TODO *)
  []
