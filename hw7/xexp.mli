(*
 * SNU 4190.310 Programming Languages 
 * Xexp Language Definition and Interpreter
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

type xexp = 
  | Num of int
  | Var of string
  | Fn of string * xexp
  | App of xexp * xexp
  | If of xexp * xexp * xexp
  | Equal of xexp * xexp
  | Raise of xexp
  | Handle of xexp * int * xexp

type closure
type value =
  | N of int
  | B of bool
  | C of closure

type result = 
  | Val of value  (* Value *)
  | Exn of int    (* Exception *)

val run : xexp -> result
val print : xexp -> unit
val is_sugarless : xexp -> bool

