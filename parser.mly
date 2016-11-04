/* File parser.mly */
%{
open Ast
%}
%token <string> NAME
%token <string> VARS
%token <int> INT
%token <bool> TRUE
%token <bool> FALSE
%token PLUS MINUS TIMES DIV
%token GT EQUALS AND NOT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token FUNC GO
%token SEMICOLON COMMA
%token SEND NEWCHANNEL
%token DECLARE ASSIGN
%token WHILE
%token IF ELSE
%token RETURN
%token PRINT
%token INTTYPE BOOLTYPE CHANINTTYPE
%token EOF

%left AND               /* lowest precedence */
%left EQUAL             /* lowest precedence */
%left GT                /* lowest precedence */
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */

%start main             /* the entry point */
%type <Ast.prog> main
%%
main:
    prog EOF                { $1 }
;
prog:
  | block                   { Prog([], $1)}
  | procs block             { Prog($1, $2)}
;
procs:
    proc        { [$1] }
  | proc procs  { $1 :: $2 }
proc:
    FUNC NAME LPAREN RPAREN block             { Proc($2, [], None, Locals [], $5) }
  | FUNC NAME LPAREN param RPAREN block       { Proc($2, $4, None, Locals [], $6) }
  | FUNC NAME LPAREN RPAREN dtype block       { Proc($2, [], Some($5), Locals [], $6) }
  | FUNC NAME LPAREN param RPAREN dtype block { Proc($2, $4, Some($6), Locals [], $7) }
;
param:
    param COMMA NAME dtype { (Var($3), $4) :: $1 }
  | NAME dtype { [(Var($1), $2)] }
;
block:
    LBRACE statement RBRACE { $2 }
;
statement:
    statement SEMICOLON statement { Seq($1, $3) }
  | GO block { Go($2) }
  | NAME SEND aexp { Transmit($1, $3) }
  | SEND NAME { RcvStmt($2) }
  | NAME DECLARE bexp { Decl(None, $1, $3) }
  | NAME DECLARE NEWCHANNEL { DeclChan($1)}
  | NAME ASSIGN bexp { Assign($1, $3) }
  | WHILE bexp block { While($2, Locals [], $3) }
  | IF bexp block ELSE block { ITE($2, Locals [], $3, Locals [], $5) }
  | RETURN bexp { Return($2) }
  | NAME LPAREN RPAREN { FuncCall($1, []) }
  | NAME LPAREN arg RPAREN { FuncCall($1, $3) }
  | PRINT bexp { Print($2) }
;
bexp:
    cexp { $1 }
  | cexp AND cexp { And($1, $3) }
;
cexp:
    cterm { $1 }
  | cterm EQUALS cterm { Eq($1, $3) }
;
cterm:
    aexp { $1 }
  | aexp GT aexp { Gt($1, $3) }
;
aexp:
    term { $1 }
  | term PLUS term { Plus($1, $3) }
  | term MINUS term { Minus($1, $3) }
;
term:
    factor { $1 }
  | factor TIMES factor { Times($1,$3) }
  | factor DIV factor { Division($1,$3) }
;
factor:
    INT { IConst($1) }
  | TRUE { BConst($1) }
  | FALSE { BConst($1) }
  | NAME { Var($1) }
  | SEND NAME { RcvExp($2) }
  | NOT factor { Not($2) }
  | LPAREN bexp RPAREN { $2 }
  | NAME LPAREN RPAREN { FuncExp($1, []) }
  | NAME LPAREN arg RPAREN { FuncExp($1, $3) }
;
arg:
    bexp { [$1] }
  | bexp COMMA arg { [] }
;
dtype:
    INTTYPE { TyInt }
  | BOOLTYPE { TyBool }
  | CHANINTTYPE { TyChan(TyInt) }
;
vars:
    VARS { Var($1) }
;
