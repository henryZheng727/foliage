%{
  open Syntax
%}

%token <string> ATOM
%token AND OR IMPLIES NOT FALSE
%token LPAREN RPAREN
%token EOF

%right IMPLIES
%left OR
%left AND
%nonassoc NOT

%type <Syntax.prop> main
%start main

%%

main:
  expr EOF { $1 }

expr:
  | ATOM                    { Atom $1 }
  | FALSE                   { False }
  | NOT expr                { Not $2 }
  | expr AND expr           { And ($1, $3) }
  | expr OR expr            { Or ($1, $3) }
  | expr IMPLIES expr       { Implies ($1, $3) }
  | LPAREN expr RPAREN      { $2 }
