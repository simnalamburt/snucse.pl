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
  | TComparableVar of var
  | TPrintableVar of var
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
    | TVar v | TComparableVar v | TPrintableVar v -> [v]
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

let make_subst (query: var) (toutput: typ): subst =
  let rec subst (tinput: typ): typ =
    match tinput with
    | TInt | TBool | TString -> tinput
    | TPair (tleft, tright) -> TPair (subst tleft, subst tright)
    | TLoc tinner -> TLoc (subst tinner)
    | TFun (tfunc, tparam) -> TFun (subst tfunc, subst tparam)
    | TVar name | TComparableVar name | TPrintableVar name when not (query = name) -> tinput
    (* Precondition: query = name *)
    | TVar name -> toutput
    | TComparableVar name -> begin
      match toutput with
      | TVar x -> TComparableVar x
      | _ -> toutput
    end
    | TPrintableVar name -> begin
      match toutput with
      | TVar x -> TPrintableVar x
      | TComparableVar x -> TPrintableVar x
      | _ -> toutput
    end
  in
  subst

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
    | TVar name | TComparableVar name | TPrintableVar name -> query = name
  in
  match left, right with
  | left, right when left = right -> empty_subst

  | TVar alpha, ty
  | ty, TVar alpha when not (occurs alpha ty) -> make_subst alpha ty

  | TComparableVar alpha, ty
  | ty, TComparableVar alpha when not (occurs alpha ty) -> begin
    match ty with
    | TInt | TBool | TString | TLoc _ | TComparableVar _ | TPrintableVar _ ->
      make_subst alpha ty
    | TPair _ | TFun _ | TVar _ ->
      raise (M.TypeError "Tried to compare uncomparable types")
  end

  | TPrintableVar alpha, ty
  | ty, TPrintableVar alpha when not (occurs alpha ty) -> begin
    match ty with
    | TInt | TBool | TString | TPrintableVar _ ->
      make_subst alpha ty
    | TPair _ | TLoc _ | TFun _ | TVar _ | TComparableVar _ ->
      raise (M.TypeError "Tried to print unprintable types")
  end

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
  | TFun _ | TVar _ | TComparableVar _ | TPrintableVar _ -> raise (M.TypeError "Wrong Input")

let check (input: M.exp): M.typ =
  let rec m (env: typ_env) (input: M.exp) (expected: typ): subst =
    match input with
    | M.CONST M.S _ -> unify expected TString
    | M.CONST M.N _ -> unify expected TInt
    | M.CONST M.B _ -> unify expected TBool
    | M.VAR name -> begin
      let tyscheme =
        try List.assoc name env with
        | Not_found -> raise (M.TypeError ("Undefined Variable \x1b[33m" ^ name ^ "\x1b[0m"))
      in
      let typ = match tyscheme with
      | SimpleTyp typ -> typ
      | GenTyp (alphas, typ) -> begin
        (* TODO: 정리 *)
        let GenTyp (_, typ) = subst_scheme empty_subst tyscheme in
        typ
      end in
      unify expected typ
    end
    | M.FN (name, ebody) -> begin
      let vparam, vret = TVar (new_var ()), TVar (new_var ()) in

      let subst1 = unify expected (TFun (vparam, vret)) in
      let env = subst_env subst1 env in

      let subst2 = m ([name, SimpleTyp (subst1 vparam)] @ env) ebody (subst1 vret) in
      subst2 @@ subst1
    end
    | M.APP (efunc, eparam) -> begin
      let beta = TVar (new_var ()) in

      let subst1 = m env efunc (TFun (beta, expected)) in
      let beta, env, expected = subst1 beta, subst_env subst1 env, subst1 expected in

      let subst2 = m env eparam beta in
      subst2 @@ subst1
    end
    | M.LET (M.VAL (name, edef), erest) -> begin
      let beta = TVar (new_var ()) in

      let subst1 = m env edef beta in
      let beta, env, expected = subst1 beta, subst_env subst1 env, subst1 expected in

      let subst2 = begin
        let tyscheme =
          if expansive edef then SimpleTyp beta
          else generalize env beta
        in
        m ([name, tyscheme] @ env) erest expected
      end in
      subst2 @@ subst1
    end
    | M.LET (M.REC (fname, pname, ebody), erest) -> begin
      let beta = TVar (new_var ()) in

      let subst1 = begin
        let env = [fname, SimpleTyp beta] @ env in
        m env (M.FN (pname, ebody)) beta
      end in
      let beta, env, expected = subst1 beta, subst_env subst1 env, subst1 expected in

      let subst2 = begin
        let env = [fname, generalize env beta] @ env in
        m env erest expected
      end in
      subst2 @@ subst1
    end
    | M.IF (econd, ethen, eelse) -> begin
      let subst1 = m env econd TBool in
      let env, expected = subst_env subst1 env, subst1 expected in

      let subst2 = m env ethen expected in
      let env, expected = subst_env subst2 env, subst2 expected in

      let subst3 = m env eelse expected in
      subst3 @@ subst2 @@ subst1
    end
    | M.BOP (op, eleft, eright) -> begin
      let binary (toperand: typ) (tret: typ) =
        let subst1 = unify expected tret in
        let toperand, env = subst1 toperand, subst_env subst1 env in

        let subst2 = m env eleft toperand in
        let toperand, env = subst2 toperand, subst_env subst2 env in

        let subst3 = m env eright toperand in
        subst3 @@ subst2 @@ subst1
      in
      match op with
      | M.ADD | M.SUB -> binary TInt  TInt
      | M.AND | M.OR  -> binary TBool TBool
      | M.EQ -> begin
        binary (TComparableVar (new_var ())) TBool
      end
    end
    | M.READ -> unify expected TInt
    | M.WRITE evalue -> begin
      let subst1 = unify expected (TPrintableVar (new_var ())) in
      let expected, env = subst1 expected, subst_env subst1 env in

      let subst2 = m env evalue expected in
      subst2 @@ subst1
    end
    | M.MALLOC einner -> begin
      let beta = TVar (new_var ()) in

      let subst1 = m env einner beta in
      let beta, env, expected = subst1 beta, subst_env subst1 env, subst1 expected in

      let subst2 = unify expected (TLoc beta) in
      subst2 @@ subst1
    end
    | M.ASSIGN (eloc, evalue) -> begin
      let subst1 = m env eloc (TLoc expected) in
      let env, expected = subst_env subst1 env, subst1 expected in

      let subst2 = m env evalue expected in
      subst2 @@ subst1
    end
    | M.BANG eloc -> m env eloc (TLoc expected)
    | M.SEQ (ebefore, eafter) -> begin
      let subst1 = begin
        let beta = TVar (new_var ()) in
        m env ebefore beta
      end in
      let env, expected = subst_env subst1 env, subst1 expected in

      let subst2 = m env eafter expected in
      subst2 @@ subst1
    end
    | M.PAIR (efirst, esecond) -> begin
      let vfirst, vsecond = TVar (new_var ()), TVar (new_var ()) in
      let subst1 = unify expected (TPair (vfirst, vsecond)) in
      let vfirst, vsecond, env = subst1 vfirst, subst1 vsecond, subst_env subst1 env in

      let subst2 = m env efirst vfirst in
      let vsecond, env = subst2 vsecond, subst_env subst2 env in

      let subst3 = m env esecond vsecond in
      subst3 @@ subst2 @@ subst1
    end
    | M.FST epair -> begin
      let beta = TVar (new_var ()) in
      m env epair (TPair (expected, beta))
    end
    | M.SND epair -> begin
      let beta = TVar (new_var ()) in
      m env epair (TPair (beta, expected))
    end
  in
  let entire = TVar (new_var ()) in
  let subst = m [] input entire in
  convert_typ (subst entire)
