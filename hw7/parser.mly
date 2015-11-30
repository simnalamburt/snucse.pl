/*
 * SNU 4190.310 Programming Languages 
 *
 * Parser of xexp for Homework "Exceptions are sugar"
 */


%token IF THEN ELSE FN RARROW RAISE HANDLE LET IN
%token PLUS MINUS LP RP EOF EQ
%token <int> NUM
%token <string> ID


%right FN RARROW let 
%left NUM 
%nonassoc IF THEN ELSE 
%left EQ ID
%nonassoc LP
%left APP

%start program
%type <Xexp.xexp> program
%type <Xexp.xexp> expr

%%
program: expr EOF {$1}
    ;
expr: 
  | LP expr RP {$2}
  | NUM {Xexp.Num $1}
  | MINUS NUM {Xexp.Num (- $2)}
  | ID {Xexp.Var ($1)}
  | FN ID RARROW expr {Xexp.Fn($2,$4)}
  | expr expr %prec APP {Xexp.App($1,$2)}
  | expr EQ expr {Xexp.Equal($1,$3)}
  | IF expr THEN expr ELSE expr {Xexp.If($2,$4,$6)}
  | RAISE expr {Xexp.Raise $2}
  | LET ID EQ expr IN expr  {Xexp.App (Xexp.Fn($2, $6), $4)}
  | expr HANDLE NUM expr {Xexp.Handle ($1, $3, $4)}
    ;
%%
