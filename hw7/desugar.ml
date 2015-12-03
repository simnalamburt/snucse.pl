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
  (* TODO *)
  let k = variable () in
  let h = variable () in
  Fn (k, Fn (h, begin
    input
  end))

let removeExn (input: xexp): xexp =
  let id = begin
    let param = variable () in
    Fn (param, Var param)
  end in
  let handler = Num 201511 in
  App (App (cps input, id), handler)
