(*
 * SNU 4190.310 Programming Languages 
 *
 * K- Interpreter
 *)

open K.K

module type KMINUS_PP =
  sig
    val pp: exp -> unit
  end

module Kminus_PP : KMINUS_PP =
  struct
    let q x = ["\"" ^ x ^ "\""]
    let pfx = "  "
    let indent l = List.map (fun s -> pfx ^ s) l
    let rec comma = function [] -> []
      | [h] -> [h ^ ","]
      | (h :: t) -> h :: (comma t)
	let rec qs xs = match xs with
		[] -> []
		| [hd] -> (q hd)
		| hd::tl -> (comma (q hd))@(qs tl)
    let ps s l = 
		match l with 
		  [] -> [s]
		| (h :: t) -> 
			(s ^ "(")
          		:: (List.fold_left (fun l x -> (comma l) @ (indent x)) (indent h) t)
          		@ [(")")]
	let rec id_e (id,e) = (q id)@(pe e)
    and pe e =
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
        | SEQ (e1, e2) -> ps "SEQ" [pe e1; pe e2]
        | IF (e1, e2, e3) -> ps "IF" [pe e1; pe e2; pe e3]
        | WHILE (e1, e2) -> ps "WHILE" [pe e1; pe e2]
        | LETV (i, e1, e2) -> ps "LETV" [q i; pe e1; pe e2]
        | LETF(f, xs, e1, e2) -> ps "LETF" 
			[q f; ps "ARGS" [qs xs]; pe e1; pe e2]
        | CALLV (f, es) -> ps "CALLV" 
			[q f; ps "ARGS" (List.map pe es)]
        | CALLR (f, ys) -> ps "CALLR" 
			[q f; ps "ARGS" [qs ys]]
		| RECORD a -> ps "RECORD" (List.map id_e a)
		| FIELD (e1,id) -> ps "FIELD" [pe e1; q id] 
        | ASSIGN (i, e) -> ps "ASSIGN" [q i; pe e]
		| ASSIGNF (e1, i, e2) -> ps "ASSIGNF" [pe e1; q i; pe e2]
        | READ i -> ps "READ" [q i]
        | WRITE e -> ps "WRITE" [pe e]
    let pp e =  List.iter print_endline (pe e)
  end
