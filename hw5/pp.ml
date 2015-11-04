(*
 * SNU 4190.310 Programming Languages 
 *
 * SM5
 *)

open K
open K
module KParseTreePrinter : sig val print : program -> unit end =
  struct
	let q x = ["\"" ^ x ^ "\""]
    let pfx = "  "
    let indent l = List.map (fun s -> pfx ^ s) l
    let rec comma = function [] -> []
      | [h] -> [h ^ ","]
      | (h :: t) -> h :: (comma t)
    let ps s l = 
		match l with
		  [] -> [s]
    	| (h :: t) -> (s ^ "(") :: (List.fold_left (fun l x -> (comma l) @ (indent x)) (indent h) t)
          @ [(")")]
  
    let rec pe e =
        match e with
          NUM i -> ps "NUM" [[string_of_int i]]
        | TRUE -> ps "TRUE" []
        | FALSE -> ps "FALSE" []
        | UNIT -> ps "UNIT" []
        | VAR x -> ps "VAR" [q x]
        | ADD (e1, e2) -> ps "ADD" [pe e1; pe e2]
        | SUB (e1, e2) -> ps "SUB" [pe e1; pe e2]
        | MUL (e1, e2) -> ps "MUL" [pe e1; pe e2]
        | DIV (e1, e2) -> ps "DIV" [pe e1; pe e2]
        | EQUAL (e1, e2) -> ps "EQUAL" [pe e1; pe e2]
        | LESS (e1, e2) -> ps "LESS" [pe e1; pe e2]
        | NOT e -> ps "NOT" [pe e]
        | ASSIGN (i, e) -> ps "ASSIGN" [q i; pe e]
        | SEQ (e1, e2) -> ps "SEQ" [pe e1; pe e2]
        | IF (e1, e2, e3) -> ps "IF" [pe e1; pe e2; pe e3]
        | WHILE (e1, e2) -> ps "WHILE" [pe e1; pe e2]
        | FOR (i, e1, e2, e3) -> ps "FOR" [q i; pe e1; pe e2; pe e3]
        | LETV (i, e1, e2) -> ps "LETV" [q i; pe e1; pe e2]
        | LETF(f, x, e1, e2) -> ps "LETF" [q f; q x; pe e1; pe e2]
        | CALLV (f, e) -> ps "CALLV" [q f; pe e]
        | CALLR (f, y) -> ps "CALLR" [q f; q y]
        | READ i -> ps "READ" [q i]
        | WRITE e -> ps "WRITE" [pe e]
     let print pgm =  List.iter print_endline (pe pgm)
  end
