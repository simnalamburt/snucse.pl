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

let removeExn (input: xexp): xexp =
  input
