(*
 * SNU 4190.310 Programming Languages 
 * SM5 Interface .mli 
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

type record
type loc 
type value = Z of int | B of bool | L of loc | Unit | R of record
type cmd = 
  | PUSH of obj 
  | POP 
  | STORE 
  | LOAD 
  | JTR of command * command
  | MALLOC 
  | BOX of int 
  | UNBOX of string 
  | BIND of string 
  | UNBIND
  | GET 
  | PUT 
  | CALL 
  | ADD 
  | SUB 
  | MUL 
  | DIV
  | EQ
  | LESS
  | NOT
and obj = Val of value | Id of string | Fn of string * command
and command = cmd list

exception Error of string

val debug_mode : bool ref

val command_to_str : string -> command -> string
val run : command -> unit
