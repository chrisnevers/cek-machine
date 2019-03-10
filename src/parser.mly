%{
  open Ast
%}

%token EOF
%token FUN
%token ARROW
%token LPAREN RPAREN
%token APP
%token ADD
%token <int> NUM
%token <string> ID

%nonassoc LPAREN ID NUM
%left APP
%left ADD

%start main

%type <Ast.m> main

%%

main:
  | e=exp EOF                       { e }

exp:
  | ID                              { Var ($1) }
  | NUM                             { Con ($1) }
  | exp ADD exp                     { Prm ("+", [$1; $3]) }
  | FUN ID ARROW exp                { Abs ($2, $4) }
  | LPAREN exp RPAREN               { $2 }
  | exp exp %prec APP               { App ($1, $2) }

