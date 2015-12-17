(*
 * SNU 4190.310 Programming Languages 2015 Fall
 * Type Checker
 *
 * Jaeseung Choi <jschoi@ropas.snu.ac.kr>
 * Hyeon Kim <simnalamburt@gmail.com>
 *)
open M

(*
 * Mainly used types
 *)
type var = string
type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
type typ_scheme =
  | SimpleTyp of typ
  | GenTyp of (var list * typ)
type typ_env = (M.id * typ_scheme) list
type subst = typ -> typ

let new_var: unit -> string = begin
  let counter = ref (-1) in
  (fun () -> begin
    counter := !counter + 1;
    "β" ^ (string_of_int !counter)
  end)
end

(*
 * Generalize given type into a type scheme
 *)
let generalize (tyenv: typ_env) (t: typ): typ_scheme =
  (* Definitions related to free type variable *)
  let union_ftv ftv_1 ftv_2 =
    let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
    ftv_1' @ ftv_2
  in
  let sub_ftv ftv_1 ftv_2 =
    List.filter (fun v -> not (List.mem v ftv_2)) ftv_1
  in
  let rec ftv_of_typ : typ -> var list = function
    | TInt | TBool | TString -> []
    | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
    | TLoc t -> ftv_of_typ t
    | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
    | TVar v -> [v]
  in
  let ftv_of_scheme : typ_scheme -> var list = function
    | SimpleTyp t -> ftv_of_typ t
    | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas
  in
  let ftv_of_env : typ_env -> var list = fun tyenv ->
    List.fold_left
      (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
      [] tyenv
  in
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(*
 * Definitions related to substitution
 *)
let empty_subst: subst = fun t -> t

let make_subst (x: var) (t: typ): subst =
  let rec subs t' =
    match t' with
    | TVar x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

(* substitution composition *)
let (@@) (sleft: subst) (sright: subst): subst =
  fun snext -> sleft (sright snext)

let subst_scheme (subs: subst) (tyscm: typ_scheme): typ_scheme =
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) -> begin
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))
  end

let subst_env (subs: subst) (tyenv: typ_env): typ_env =
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

(*
 * Type checking
 *)
let rec unify (left: typ) (right: typ): subst =
  let rec occurs (query: var) (ty: typ): bool =
    let inner = occurs query in
    match ty with
    | TInt | TBool | TString -> false
    | TPair (left, right) | TFun (left, right) -> inner left || inner right
    | TLoc value -> inner value
    | TVar name -> query = name
  in
  match left, right with
  | left, right when left = right -> empty_subst
  | TVar alpha, ty
  | ty, TVar alpha when not (occurs alpha ty) -> make_subst alpha ty
  | TPair(lparam, lbody), TPair(rparam, rbody)
  | TFun(lparam, lbody), TFun(rparam, rbody) -> begin
    let subst1 = unify lparam rparam in
    let subst2 = unify (subst1 lbody) (subst1 rbody) in
    subst2 @@ subst1
  end
  | TLoc linner, TLoc rinner -> unify linner rinner
  | _ -> raise (M.TypeError "Type Mismatch")

let rec expansive (input: M.exp): bool =
  match input with
  | M.CONST _
  | M.VAR _
  | M.FN _
  | M.READ
  -> false
  | M.MALLOC _
  | M.APP _
  -> true
  | M.WRITE einner
  | M.BANG einner
  | M.FST einner
  | M.SND einner
  -> expansive einner
  | M.LET (M.REC (_, _, e1), e2)
  | M.LET (M.VAL (_, e1), e2)
  | M.BOP (_, e1, e2)
  | M.ASSIGN (e1, e2)
  | M.SEQ  (e1, e2)
  | M.PAIR (e1, e2)
  -> expansive e1 || expansive e2
  | M.IF (econd, ethen, eelse)
  -> expansive econd || expansive ethen || expansive eelse

let rec convert_typ (input: typ): M.typ =
  match input with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (tleft, tright) -> M.TyPair (convert_typ tleft, convert_typ tright)
  | TLoc tinner -> M.TyLoc (convert_typ tinner)
  | TFun _ | TVar _ -> raise (M.TypeError "Wrong Input")

let check (input: M.exp): M.typ =
  let rec w (env: typ_env) (input: M.exp): subst * typ =
    match input with
    | M.CONST M.S _ -> empty_subst, TString
    | M.CONST M.N _ -> empty_subst, TInt
    | M.CONST M.B _ -> empty_subst, TBool
    | M.VAR name -> begin
      if List.mem_assoc name env then
        (* TODO: 정리 *)
        let tyscheme = List.assoc name env in
        match tyscheme with
        | SimpleTyp typ -> empty_subst, typ
        | GenTyp (alphas, typ) -> begin
          let GenTyp (_, typ) = subst_scheme empty_subst tyscheme in
          empty_subst, typ
        end
      else
        raise (M.TypeError "Undefined Variable")
    end
    | M.FN (name, ebody) -> begin
      let beta = TVar (new_var ()) in
      let subst1, ty1 = w ([name, SimpleTyp beta] @ env) ebody in
      subst1, subst1 (TFun (beta, ty1))
    end
    | M.APP (efunc, eparam) -> begin
      let beta = TVar (new_var ()) in
      let subst1, ty1 = w env efunc in
      let subst2, ty2 = w (subst_env subst1 env) eparam in
      let subst3 = unify (subst2 ty1) (TFun (ty2, beta)) in
      subst3 @@ subst2 @@ subst1, subst3 beta
    end
    (*
    (* TODO *)
    | M.LET of decl * exp
    | M.IF of exp * exp * exp
    | M.BOP of bop * exp * exp
    | M.READ
    | M.WRITE of exp
    | M.MALLOC of exp          (*   malloc e *)
    | M.ASSIGN of exp * exp    (*   e := e   *)
    | M.BANG of exp            (*   !e       *)
    | M.SEQ of exp * exp       (*   e ; e    *)
    | M.PAIR of exp * exp      (*   (e, e)   *)
    | M.FST of exp            (*   e.1      *)
    | M.SND of exp            (*   e.2      *)
    *)
  in
  let subst, result = w [] input in
  convert_typ (subst result)
