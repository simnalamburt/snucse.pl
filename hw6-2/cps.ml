(*
 * SNU 4190.310 Programming Languages
 * Continuation Passing Style Conversion Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open M0

let count = ref 0

let new_name () =
  let _ = count := !count + 1 in
  "x_" ^ (string_of_int !count)

let rec alpha_conv exp subs =
  match exp with
  | Num n -> Num n
  | Var x -> (try Var (List.assoc x subs) with Not_found -> Var x)
  | Fn (x, e) -> begin
    let x' = new_name () in
    let subs' = (x, x') :: subs in
    Fn (x', alpha_conv e subs')
  end
  | App (e1, e2) -> App (alpha_conv e1 subs, alpha_conv e2 subs)
  | Rec (f, x, e) -> begin
    let x' = new_name () in
    let f' = new_name () in
    let subs' = (f, f') :: (x, x') :: subs in
    Rec (f', x', alpha_conv e subs')
  end
  | Ifz (e1, e2, e3) -> begin
    Ifz (alpha_conv e1 subs, alpha_conv e2 subs, alpha_conv e3 subs)
  end
  | Add (e1, e2) -> Add (alpha_conv e1 subs, alpha_conv e2 subs)
  | Pair (e1, e2) -> Pair (alpha_conv e1 subs, alpha_conv e2 subs)
  | Fst e -> Fst (alpha_conv e subs)
  | Snd e -> Snd (alpha_conv e subs)

let rec conv (exp: mexp): mexp =
  let k = new_name () in
  match exp with
  | Num _
  | Var _ -> Fn (k, App (Var k, exp))
  | Fn (x, e) -> Fn (k, App (Var k, Fn (x, conv e)))
  | Rec (f, x, e) -> Fn (k, App (Var k, Rec (f, x, conv e)))
  | App (efunc, eparam) -> begin
    let vparam = new_name () in
    let vfunc = new_name () in
    Fn (k,
      App (conv efunc,
        Fn (vfunc,
          App (conv eparam,
            Fn (vparam,
              App (App (Var vfunc, Var vparam), Var k)
            )
          )
        )
      )
    )
  end
  | Ifz (econd, ethen, eelse) -> begin
    let cps_then =
      let vthen = new_name () in
      App (conv ethen,
        Fn (vthen,
          App (Var k, Var vthen)
        )
      )
    in
    let cps_else =
      let velse = new_name () in
      App (conv eelse,
        Fn (velse,
          App (Var k, Var velse)
        )
      )
    in
    let vcond = new_name () in
    Fn (k,
      App (conv econd,
        Fn (vcond,
          Ifz (Var vcond, cps_then, cps_else)
        )
      )
    )
  end
  | Add (e1, e2) -> begin
    (* _e1 + e2_ = λk.(_e1_ λv1.(_e2_ λv2.(k (v1 + v2)))) *)
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k,
      App (conv e1,
        Fn (v1,
          App (conv e2,
            Fn (v2,
              App (Var k, Add (Var v1, Var v2))
            )
          )
        )
      )
    )
  end
  | Pair (e1, e2) -> begin
    (* _(e1, e2)_ = λk.(_e1_ λv1.(_e2_ λv2.(k (v1, v2)))) *)
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k,
      App (conv e1,
        Fn (v1,
          App (conv e2,
            Fn (v2,
              App (Var k, Pair (Var v1, Var v2))
            )
          )
        )
      )
    )
  end
  | Fst epair -> begin
    let var = new_name () in
    Fn (k,
      App (conv epair,
        Fn (var,
          App (Var k, Fst (Var var))
        )
      )
    )
  end
  | Snd epair -> begin
    let var = new_name () in
    Fn (k,
      App (conv epair,
        Fn (var,
          App (Var k, Snd (Var var))
        )
      )
    )
  end

let cps exp = conv (alpha_conv exp [])

