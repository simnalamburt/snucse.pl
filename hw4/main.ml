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
        | T
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
  | T -> false
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
 * Utility functions from Core.Std
 *)
(* returns list without adjacent duplicates *)
let dedup_without_sorting ?(compare=Pervasives.compare) list =
  let rec loop list accum = match list with
    | [] -> accum
    | hd :: [] -> hd :: accum
    | hd1 :: hd2 :: tl ->
        if compare hd1 hd2 = 0
        then loop (hd2 :: tl) accum
        else loop (hd2 :: tl) (hd1 :: accum)
  in
  loop list []

(** returns sorted version of list with duplicates removed *)
let dedup ?(compare=Pervasives.compare) list =
  let sorted = List.sort (fun x y -> compare y x) list in
  dedup_without_sorting ~compare sorted

(* TODO: Remove debug codes *)
let rec print_ty (ty: ty) =
  match ty with
  | TyVar name -> print_string name
  | T -> print_string "T"
  | Function(tparam, tret) -> begin
    print_ty tparam;
    print_string " -> ";
    print_ty tret;
  end

(* TODO: Remove debug codes *)
let print_tyenv (tyenv: tyenv) =
  if List.length tyenv = 0 then
    print_endline "[]"
  else begin
    print_string "[";
    List.iter (fun (name, ty) -> begin
      print_string name;
      print_string ": ";
      print_ty ty;
      print_string ", ";
    end) tyenv;
    print_endline "\b\b]"
  end

(* TODO: Remove debug codes *)
let print_subst (subst: substitution) =
  if List.length subst = 0 then
    print_endline "[]"
  else begin
    print_string "[";
    List.iter (fun (name, ty) -> begin
      print_string name;
      print_string ": ";
      print_ty ty;
      print_string ", ";
    end) subst;
    print_endline "\b\b]"
  end


(*
 * Interface
 *)
let rec map_to_exp (map: map): expression =
  match map with
  | End StarBox -> Term Number
  | End NameBox name -> Term(Variable name)
  | Branch(mleft, mright) -> FnCall(map_to_exp mleft, map_to_exp mright)
  | Guide(name, minner) -> FnDef(name, map_to_exp minner)

let getReady (map: map): key list =
  (* Stateful functions *)
  let tyenv: tyenv ref = ref [] in
  let names = ref [] in
  let new_variable: unit -> ty =
    let counter = ref 0 in
    fun () -> begin
      let str = Printf.sprintf "α%d" !counter in
      counter := !counter + 1;
      TyVar str
    end
  in
  (* Inference type of given expression *)
  let inference (exp: expression): substitution =
    (*
     * The *M* Algorithm
     *
     * LEE, Oukseh; YI, Kwangkeun. Proofs about a folklore let-polymorphic type
     * inference algorithm. ACM Transactions on Programming Languages and Systems
     * (TOPLAS), 1998, 20.4: 707-723.
     *)
    let rec m_algorithm (exp: expression) (ty: ty): substitution =
      match exp with
      | Term Number -> unify T ty
      | Term Variable x -> begin
        if List.mem_assoc x !tyenv then
          let tright = List.assoc x !tyenv in
          unify ty tright
        else
          unify ty (TyVar x)
      end
      | FnDef(name, edef) -> begin
        let alpha1 = new_variable () in
        let alpha2 = new_variable () in
        let subst1 = unify (Function(alpha1, alpha2)) ty in
        tyenv := apply_env subst1 !tyenv @ [(name, apply subst1 alpha1)];
        let subst2 = m_wrapped edef (apply subst1 alpha2) in
        subst2 @ subst1 (* Note: tyvar 중복체크 안해도 됨 *)
      end
      | FnCall(efunc, eparam) -> begin
        let alpha = new_variable () in
        let subst1 = m_wrapped efunc (Function(alpha, ty)) in
        tyenv := apply_env subst1 !tyenv;
        let subst2 = m_wrapped eparam (apply subst1 alpha) in
        subst2 @ subst1 (* Note: tyvar 중복체크 안해도 됨 *)
      end
    and m_wrapped (exp: expression) (ty: ty): substitution =
      match exp with
      | Term term -> begin
        (* Note: (List.length result) is always 1 *)
        let result = m_algorithm exp ty in
        let ids, _ = List.split result in
        names := !names @ ids;
        result
      end
      | _ -> m_algorithm exp ty
    in
    m_wrapped exp (new_variable ())
  in
  let exp = map_to_exp map in
  let subst = inference exp in
  let names = List.map (fun id -> TyVar id) !names in
  let rec ty_to_key (ty: ty): key =
    match ty with
    | T -> Bar
    | Function(tleft, tright) -> Node(ty_to_key tleft, ty_to_key tright)
    | TyVar name -> begin
      if List.mem_assoc name subst then
        ty_to_key (List.assoc name subst)
      else
        (* Note: Undefined variable은 Bar로 간주 *)
        Bar
    end
  in
  let keys = List.map ty_to_key names in
  dedup keys
