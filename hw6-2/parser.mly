/*
 * SNU 4190.310 Programming Languages 
 *
 * Parser for M0
 */

%{
exception EmptyBinding
exception IncorrectSelection
let whichSel = function (e, 1) -> M0.Fst e
      | (e, 2) -> M0.Snd e
      | _ -> raise IncorrectSelection
%}
  
%token IF THEN ELSE FN RARROW DOT
%token PLUS MINUS LP RP REC COMMA EOF
%token <int> NUM
%token <string> ID


%right FN RARROW DOT REC 
%left NUM
%nonassoc IF THEN ELSE 
%left PLUS ID
%nonassoc LP
%left APP

%start program
%type <M0.mexp> program
%type <M0.mexp> expr

%%
program: expr EOF {$1}
    ;
expr: 
  | LP expr RP {$2}
  | NUM {M0.Num $1}
  | MINUS NUM {M0.Num (- $2)}
  | ID {M0.Var ($1)}
  | FN ID RARROW expr {M0.Fn($2,$4)}
  | REC ID ID RARROW expr {M0.Rec($2, $3, $5)}
  | expr expr %prec APP {M0.App($1,$2)}
  | expr PLUS expr {M0.Add($1,$3)}
  | expr DOT NUM {whichSel ($1,$3)}
  | IF expr THEN expr ELSE expr {M0.Ifz($2,$4,$6)}
  | LP expr COMMA expr RP {M0.Pair ($2, $4)}
    ;        
%%
