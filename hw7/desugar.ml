(*
 * SNU 4190.310 Programming Languages
 * Homework "Exceptions are sugar" Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)
open Xexp

let count = ref 0
let variable () =
  count := !count + 1;
  "α" ^ string_of_int !count

let rec cps (input: xexp): xexp =
  let k, h = variable (), variable () in
  let biapp (efunc: xexp) (efirst: xexp) (esecond: xexp): xexp =
    App (App (efunc, efirst), esecond)
  in
  Fn (k, Fn (h, begin
    match input with
    | Num _ | Var _ -> App (Var k, input)
    | Fn (param, ebody) -> App (Var k, Fn (param, cps ebody))
    | App (efunc, eparam) -> begin
      let f, v = variable (), variable () in
      biapp (cps efunc) begin
        Fn (f, begin
          biapp (cps eparam) begin
            Fn (v, begin
              App (App (App (Var f, Var v), Var k), Var h)
            end)
          end (Var h)
        end)
      end (Var h)
    end
    | If (econd, ethen, eelse) -> begin
      let foo (exp: xexp): xexp =
        let tmp = variable () in
        let body = Fn (tmp, App (Var k, Var tmp)) in
        biapp (cps exp) body (Var h)
      in
      let vcond = variable () in
      let body = Fn (vcond, If (Var vcond, foo ethen, foo eelse)) in
      biapp (cps econd) body (Var h)
    end
    | Equal (eleft, eright) -> begin
      (*
      cps<left+right> = k -> { h -> {
        // body
        cps<left> (val1 -> {
          cps<right> (val2 -> {
            k (val1 + val2)
          }) h
        }) h
      }};
      *)

      let val1, val2 = variable (), variable () in
      biapp (cps eleft) (Fn (val1, begin
        biapp (cps eright) (Fn (val2, begin
          App (Var k, Equal (Var val1, Var val2))
        end)) (Var h)
      end)) (Var h)
    end
    (* TODO *)
    | _ -> input
  end))

let removeExn (input: xexp): xexp =
  let id = begin
    let param = variable () in
    Fn (param, Var param)
  end in
  let handler = Num 201511 in
  App (App (cps input, id), handler)
