(*
 * SNU 4190.310 Programming Languages 
 *
 * Lexer for M
 *)

{
 open Parser
 open Error
 let debug_tag = false
 let verbose s =  if debug_tag then (print_string s; print_newline())
 let comment_depth = ref 0
 let keyword_tbl = Hashtbl.create 31
 let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
                   [("true", TRUE);
                    ("false", FALSE);
                    ("and", AND);
                    ("or", OR);
                    ("if", IF);
                    ("then",THEN);
                    ("else",ELSE);
                    ("let", LET);
                    ("in", IN);
                    ("end", END);
                    ("fn", FN);
                    ("read" , READ);
                    ("rec" , REC);
                    ("write", WRITE);
                    ("malloc", MALLOC);
                    ("val", VAL)
                  ]
     
     let s2int = function "" -> raise (Lex_err("illegal number token", get_ln()))
       		   | s -> if ('~' = String.get s 0) then
                   - (int_of_string(String.sub s 1 ((String.length s)-1)))
                   else int_of_string s
} 

let blank = [' ' '\t']+
let id = ['a'-'z' 'A'-'Z'](['a'-'z' 'A'-'Z' '\'' '0'-'9' '_'])*
let number = ['0'-'9']+|'~'['0'-'9']+
let charstring = '"' [^ '"' '\n']* '"'

rule start =
 parse blank { start lexbuf }
     | "\r\n"     { incr_ln (); start lexbuf}
     | '\n'       { incr_ln (); start lexbuf}
     | number { NUM (s2int(Lexing.lexeme lexbuf)) }
     | id { let id = Lexing.lexeme lexbuf
            in try Hashtbl.find keyword_tbl id
               with _ -> ID id
            }
     | charstring { STRING(Lexing.lexeme lexbuf) }         
     | eof { verbose "eof"; EOF}
     | "(*" { comment_depth :=1;
              comment lexbuf;
              start lexbuf }
     | ";" { verbose ";"; SEMICOLON}
     | ":=" { verbose ":="; COLONEQ}
     | "=>" { verbose "=>"; RARROW}
     | "=" {verbose "="; EQUAL}
     | "+" {verbose "+"; PLUS}
     | "-" {verbose "-"; MINUS}
     | "!" {verbose "!"; BANG}
     | "(" { verbose "("; LP}
     | ")" { verbose ")"; RP}
     | "." { verbose "."; DOT}
     | "," { verbose ","; COMMA}
     | _ {raise (Lex_err("illical token "^(Lexing.lexeme lexbuf), get_ln()))} 

and comment = parse
     "(*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*)" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise (Lex_err("Eof within comment",get_ln()))}
   | '\n' {incr_ln(); comment lexbuf}
   | "\r\n" {incr_ln(); comment lexbuf}
   | _   {comment lexbuf}
