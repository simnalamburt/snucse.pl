exception IMPOSSIBLE

type tyvar = string
type ty = TyVar of tyvar
        | Constant
        | Function of ty * ty

type substitution = (tyvar * ty) list

let rec occurs (var: tyvar) (ty: ty): bool =
  match ty with
  | TyVar(right) -> var = right
  | Constant -> false
  | Function(_, ret) -> occurs var ret

let rec apply (subst: substitution) (ty: ty): ty =
  match ty with
  | TyVar(right) -> begin
    List.assoc right subst
  end
  | Constant -> Constant
  | Function(_, ret) -> apply subst ret

let rec unify (left: ty) (right: ty): substitution =
  if left = right then
    []
  else
    match left, right with
    | TyVar(alpha), ty
    | ty, TyVar(alpha) -> begin
      if occurs alpha ty then
        raise IMPOSSIBLE
      else
        [(alpha, ty)]
    end
    | Function(tleft1, tleft2), Function(tright1, tright2) -> begin
      let subst1 = unify tleft1 tright1 in
      let subst2 = unify (apply subst1 tleft2) (apply subst1 tright2) in
      subst2 @ subst1 (* Note: tyvar 중복체크 안해도 됨 *)
    end
    | _ -> raise IMPOSSIBLE






(*
type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure
         | Branch of map * map
         | Guide of string * map


module Dict = Map.Make(String)
type dict = key Dict.t

let getReady (expression: map): key list =
  let rec inference (expression: map) (env: dict): key =
    match expression with
    | End(value) -> begin
      (* Term *)
      match value with
      | StarBox -> (* Constant *) Bar
      | NameBox(name) -> begin
        (* Variable *)
        try Dict.find name env
        with Not_found -> Bar
      end
    end
    | Branch(efunc, eparam) -> begin
      let tfunc = inference efunc env in
      match tfunc with
      | Bar -> raise IMPOSSIBLE
      | Node(tparam_expected, treturn) -> begin
        let tparam_actual = inference eparam env in
        if tparam_expected = tparam_actual then
          treturn
        else
          raise IMPOSSIBLE
      end
    end
    | Guide(name, body) -> begin
      (* Function definition, TODO *)
      Bar
    end
  in
  [inference expression Dict.empty]
*)
