(*
 * SNU 4190.310 Programming Languages 
 *
 * Lexer for M0
 *)

{
 open Parser
 exception Lex_err of string
 let debug_tag = false
 let verbose s =  if debug_tag then (print_string s; print_newline())
 let comment_depth = ref 0
 let keyword_tbl = Hashtbl.create 31
 let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
                   [("ifzero", IF);
                    ("then",THEN);
                    ("else",ELSE);
                    ("fn", FN);
                    ("rec" , REC);
                  ]
 let s2int = function "" -> raise (Lex_err("illegal number token"))
           | s -> if ('~' = String.get s 0) then
                   - (int_of_string(String.sub s 1 ((String.length s)-1)))
                   else int_of_string s
} 

let blank = [' ' '\t' '\r' '\n']+
let id = ['a'-'z' 'A'-'Z'](['a'-'z' 'A'-'Z' '\'' '0'-'9' '_'])*
let number = ['0'-'9']+|'~'['0'-'9']+

rule start =
parse blank { start lexbuf }
    | "\r\n"     { start lexbuf}
    | '\n'       { start lexbuf}
    | number { NUM (s2int(Lexing.lexeme lexbuf)) }
    | id { let id = Lexing.lexeme lexbuf
           in try Hashtbl.find keyword_tbl id
              with _ -> ID id
         }
    | eof { verbose "eof"; EOF}
    | "(*" { comment_depth :=1;
             comment lexbuf;
             start lexbuf }
    | "=>" { verbose "=>"; RARROW}
    | "+" {verbose "+"; PLUS}
    | "-" {verbose "-"; MINUS}
    | "." {verbose "."; DOT}
    | "(" { verbose "("; LP}
    | ")" { verbose ")"; RP}
    | "," {verbose ","; COMMA}
    | _ {raise (Lex_err("illical token "^(Lexing.lexeme lexbuf)))} 

and comment = parse
     "(*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*)" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise (Lex_err("Eof within comment"))}
   | '\n' { comment lexbuf}
   | "\r\n" { comment lexbuf}
   | _   {comment lexbuf}
