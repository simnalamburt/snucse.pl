(*
 * SNU 4190.310 Programming Languages 
 * M0 Language Definition and Interpreter
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

type mexp = 
  | Num of int
  | Var of id
  | Fn of id * mexp
  | App of mexp * mexp
  | Rec of id * id * mexp
  | Ifz of mexp * mexp * mexp
  | Add of mexp * mexp
  | Pair of mexp * mexp      (* (e, e) *)
  | Fst of mexp            (*   e.1  *)
  | Snd of mexp            (*   e.2  *)
and id = string

type closure
type value = 
  | N of int
  | P of value * value
  | C of closure

val run : mexp -> value 
val print : mexp -> unit
