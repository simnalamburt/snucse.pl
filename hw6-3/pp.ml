(*
 * SNU 4190.310 Programming Languages 
 *
 * M pretty printer 
 *)

open M.M

module M_Printer :
sig
  val print : exp -> unit
end =
struct
  let ps = print_string
  let nl = print_newline
  let indent i =
    let rec it = function 
      | 0 -> ()
      |  n -> ps " "; it (n-1)
    in  
    nl (); it i

  let rec pp n =
    function CONST (S s) -> ps s
    | CONST (N m) -> print_int m
    | CONST (B true) -> ps "true"
    | CONST (B false) -> ps "false"
    | VAR s -> ps s
    | FN (x, e) -> ps ("fn " ^ x ^ " -> "); (
            match e with
              FN _ -> pp (n+1) e
            | _ -> indent (n+1); pp (n+1) e
        )
    | APP (e, e') -> pp n e; ps " "; pp n e'
    | IF (e1, e2, e3)-> ps "if "; pp n e1; ps " then ";
                     indent (n+1); pp (n+1) e2;
                     indent (n); ps "else";
                     indent (n+1); pp (n+1) e3
    | READ -> ps "read "
    | WRITE (e) -> ps "write("; pp n e; ps ")"
    | LET (d, e) ->
      let rec sugaring l acc =
        match l with
        | LET (d, LET (d', e)) -> sugaring (LET (d', e)) (d::acc)
        | LET (d, e) -> (List.rev (d::acc), e)
        | _  ->  raise (Invalid_argument "impossible")
      in
      let (decls, body) = sugaring (LET (d, e)) [] in
      ps "let ";
      List.iter (fun x -> (indent(n+1); printDecl (n+1) x)) decls;
      indent n; ps "in";
      indent (n+1); pp (n+1) body;
      indent n; ps "end"
    | MALLOC e -> ps "malloc "; pp (n+1) e
    | ASSIGN (e, e') -> pp n e; ps " := ";  pp n e'
    | BANG e -> ps "!"; pp n e
    | SEQ (e, e') -> pp n e; ps ";"; indent n; pp n e'
    | PAIR (e1, e2) -> ps "("; pp n e1; ps ", "; pp n e2; ps ")"
    | FST e -> pp n e; ps ".1"
    | SND e -> pp n e; ps ".2"
    | BOP (op, e1, e2) -> 
      let op_str = 
        (match op with 
        | ADD -> "+" 
        | SUB -> "-" 
        | EQ -> "="
        | AND -> "and" 
        | OR -> "or")
      in
      ps "("; pp n e1; ps (" " ^ op_str ^ " "); pp n e2; ps ")"
  and printDecl n = function 
    | VAL (x, e) -> ps "val "; ps (x ^ " = "); pp (n+1) e
    | REC (f, x, e) -> ps ("rec " ^ f ^ "(" ^ x ^ ") = "); pp (n+1) e
  
  let rec pp_type ty = 
    match ty with
    | TyInt -> ps "int"
    | TyBool -> ps "bool"
    | TyString  -> ps "string"
    | TyPair (tau1, tau2) ->
        ps "("; pp_type tau1; ps " , "; pp_type tau2; ps ")"
    | TyLoc tau1 -> ps "loc("; pp_type tau1; ps ")"
    | TyArrow (tau1, tau2) ->
      ps "("; pp_type tau1; ps ")" ; ps "->";
      ps "("; pp_type tau2; ps ")"

  let print = pp 0
  let printTypes ty = pp_type ty; nl()
end
