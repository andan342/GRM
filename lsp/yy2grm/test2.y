//Example from "flex & bison" book by John Levine, p.59

%{
#include <stdio.h>
#include <stdlib.h>
#include "fb3-1.h" 
%}

%union {
  struct ast *a;
  double d;
}

%token <d> NUMBER
%token EOL

%left '+' '-'
%left '*' '/'
%nonassoc UMINUS

%type <a> exp

%%
exp: exp '+' exp { $$ = newast('+', $1,$3); }
   | exp '-' exp { $$ = newast('-', $1,$3); }
   | exp '*' exp { $$ = newast('*', $1,$3); }
   | exp '/' exp { $$ = newast('/', $1,$3); }
   | '-' exp %prec UMINUS { $$ = newast('M', $2, NULL); } //possible mistype corrected
   | '(' exp ')' { $$ = $2; }
   | NUMBER { $$ = newnum($1); }
   ;

