/* File parser.mly */
%{
open Ast
%}
%token <char> LETTER
%token <string> NAME
%token <string> VARS
%token <int> INT
%token <bool> BOOL
%token FUNC
%token PLUS MINUS TIMES DIV
%token GT EQUALS AND NOT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token FUNC GO
%token SEMICOLON COMMA
%token SEND NEWCHANNEL
%token DECLARE OVERRIDE
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
    proc block              { Prog($1, $2) }
;
proc:
    FUNC name LPAREN RPAREN block { [Proc($2, [], None, $5)] }
  | FUNC name LPAREN param RPAREN block { [Proc($2, [], None, $6)]  }
  | FUNC name LPAREN RPAREN dtype block {  [Proc($2, [], None, $6)] }
  | FUNC name LPAREN param RPAREN dtype block {  [Proc($2, [], None, $7)] }
;
param:
    vars dtype {}
  | param COMMA vars dtype {}
;
block:
    LBRACE statement RBRACE {}
statement:
    statement SEMICOLON statement {}
  | GO block {}
  | vars SEND aexp {}
  | SEND vars {}
  | vars DECLARE bexp {}
  | vars DECLARE NEWCHANNEL {}
  | vars OVERRIDE bexp {}
  | WHILE bexp block {}
  | IF bexp block ELSE block {}
  | RETURN bexp {}
  | name LPAREN arg RPAREN {}
  | PRINT bexp {}
;
bexp:
    cexp {}
  | cexp AND bexp {}
;
cexp:
    cterm {}
  | cterm EQUALS cexp {}
;
cterm:
    aexp {}
  | aexp GT aexp {}
;
aexp:
    term {}
  | term PLUS aexp {}
  | term MINUS aexp {}
;
term:
    factor {}
  | factor TIMES term {}
  | factor DIV term {}
;
factor:
    INT {}
  | BOOL {}
  | vars {}
  | SEND vars {}
  | NOT factor {}
  | LPAREN bexp RPAREN {}
  | name LPAREN arg RPAREN {}
;
arg:
    bexp {}
  | bexp COMMA arg {}
;
dtype:
    INTTYPE {}
  | BOOLTYPE {}
  | CHANINTTYPE {}
;
name:
    NAME {}
;
vars:
    VARS {}
;
