(*
 * SNU 4190.310 Programming Languages 
 * K-- Interpreter
 *)

{
 open Parser
 exception Eof
 exception LexicalError
 let verbose1 s =  (* (print_string s; print_newline(); s) *) s
 let verbose2 s =  (* (print_string s; print_newline()) *) ()
 let comment_depth = ref 0
 let keyword_tbl = Hashtbl.create 31
 let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
                   [("unit", UNIT);
				    ("true", TRUE);
                    ("false", FALSE);
                    ("not", NOT);
                    ("if", IF);
                    ("then",THEN);
                    ("else",ELSE);
                    ("let", LET);
                    ("in", IN);
                    ("end", END);
	    	        ("proc", PROC);
                    ("while", WHILE);
                    ("do"   , DO);
                    ("for"  , FOR);
                    ("to"   , TO);
                    ("read" , READ);
                    ("write", WRITE)
                  ] 
} 

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*
let number = ['0'-'9']+

rule start =
 parse blank { start lexbuf }
     | "(*" { comment_depth :=1;
              comment lexbuf;
              start lexbuf }
     | number { NUM (int_of_string (verbose1 (Lexing.lexeme lexbuf))) }
     | id { let id = verbose1 (Lexing.lexeme lexbuf)
            in try Hashtbl.find keyword_tbl id
               with _ -> ID id
            }
     | "+" {verbose2 "+"; PLUS}
     | "-" {verbose2 "-";MINUS}
     | "*" { verbose2 "*"; STAR}
     | "/" { verbose2 "/"; SLASH}
     | "=" {verbose2 "="; EQUAL}
     | "<" { verbose2 "<"; LB}
     | ">" { verbose2 ">"; RB}
     | "]" { verbose2 "]"; RBLOCK}
     | "[" { verbose2 "["; LBLOCK}
     | ":=" {verbose2 ":="; COLONEQ}
     | ";" { verbose2 ";"; SEMICOLON}
     | "(" { verbose2 "("; LP}
     | ")" { verbose2 ")"; RP}
     | eof { verbose2 "eof"; EOF}
     | _ {raise LexicalError}

and comment = parse
     "(*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*)" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise Eof}
   | _   {comment lexbuf}
