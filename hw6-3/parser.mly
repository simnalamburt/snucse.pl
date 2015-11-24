/*
 * SNU 4190.310 Programming Languages 
 *
 * Parser for M
 */

%{ 
exception EmptyBinding
let rec desugarLet =
  function ([], e) -> raise EmptyBinding
   | (a::[], e) -> M.M.LET(a,e)
   | (a::r, e) -> M.M.LET(a, desugarLet(r,e))

exception IncorrectSelection
let whichSel = function (e, 1) -> M.M.FST e
       | (e, 2) -> M.M.SND e
       | _ -> raise IncorrectSelection
%}
%token TRUE FALSE AND OR IF THEN ELSE LET IN END FN READ WRITE RARROW EQUAL
%token PLUS MINUS LP RP VAL COLONEQ BANG MALLOC SEMICOLON REC EOF DOT COMMA
%token <int> NUM
%token <string> ID
%token <string> STRING

%left SEMICOLON         
%right FN RARROW LET 
%right WRITE
%right COLONEQ         
%nonassoc IF THEN ELSE
%left EQUAL 
%left PLUS MINUS OR
%left AND
%right BANG MALLOC
%left DOT 
%nonassoc TRUE FALSE NUM ID STRING READ LP

%start program
%type <M.M.exp> program
%type <M.M.exp> expr
%type <M.M.decl> decl         

%%
program: expr EOF   {$1}
    ;
expr: aexpr {$1}
    | expr aexpr {M.M.APP($1,$2)}
    | expr PLUS expr {M.M.BOP(M.M.ADD,$1,$3)}
    | expr MINUS expr {M.M.BOP(M.M.SUB,$1,$3)}
    | expr EQUAL expr {M.M.BOP(M.M.EQ,$1,$3)}
    | expr AND expr {M.M.BOP(M.M.AND,$1,$3)}
    | expr OR expr {M.M.BOP(M.M.OR,$1,$3)}
    | expr SEMICOLON expr {M.M.SEQ ($1,$3)}
    | expr COLONEQ expr {M.M.ASSIGN($1,$3)}
    | expr DOT NUM {whichSel ($1,$3)}
    ;
aexpr: LP expr RP {$2}
    | NUM {M.M.CONST(M.M.N $1)}
    | STRING {M.M.CONST(M.M.S $1)}
    | TRUE {M.M.CONST(M.M.B true)}
    | FALSE {M.M.CONST(M.M.B false)}
    | ID {M.M.VAR($1)}
    | READ {M.M.READ}
    | FN ID RARROW expr {M.M.FN($2,$4)}
    | LET decls IN expr END {desugarLet($2,$4)}
    | IF expr THEN expr ELSE expr {M.M.IF($2,$4,$6)}
    | WRITE expr {M.M.WRITE ($2)}
    | MALLOC expr {M.M.MALLOC ($2)}
    | BANG expr {M.M.BANG ($2)}
    | LP expr COMMA expr RP {M.M.PAIR ($2,$4)}
    ;        
decls: decl {[$1]}
    | decls decl {$1 @ [$2]}
    ;
decl: VAL ID EQUAL expr {M.M.VAL($2, $4)}
    | REC ID EQUAL FN ID RARROW expr {M.M.REC($2, $5, $7)}
    ;
%%
