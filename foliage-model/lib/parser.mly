%{
  open Types
%}

%token <string> ATOM
%token <int> NUMBER
%token AND OR IMPLIES NOT FALSE TRUE LPAREN RPAREN
%token PIPE BY COMMA COLON DASH
%token PREMISE ASSUMPTION REIT ANDI ANDEL ORI ORE IMPLIESI IMPLIESE
%token EOF

%right IMPLIES
%left OR
%left AND
%nonassoc NOT

%type <Types.prop> expr
%type <Types.prop> main
%type <Types.line_ref> line_ref
%type <Types.rule> justification
%start main

%%

main:
  expr EOF { $1 }

expr:
  | ATOM                    { Atom $1 }
  | FALSE                   { False }
  | TRUE                    { True }
  | NOT expr                { Implies ($2, False) }
  | expr AND expr           { And ($1, $3) }
  | expr OR expr            { Or ($1, $3) }
  | expr IMPLIES expr       { Implies ($1, $3) }
  | LPAREN expr RPAREN      { $2 }

line_ref:
  | NUMBER                  { Single $1 }
  | NUMBER DASH NUMBER      { Range ($1, $3) }

justification:
  | PREMISE                 { Premise }
  | ASSUMPTION              { Assumption }
  | REIT COLON line_ref     { Reit $3 }
  | ANDI COLON line_ref COMMA line_ref { AndI ($3, $5) }
  | ANDEL COLON line_ref    { AndEL $3 }
  | ORI COLON line_ref      { OrIL $3 }
  | ORE COLON line_ref COMMA line_ref COMMA line_ref { OrE ($3, $5, $7) }
  | IMPLIESI COLON line_ref { ImpliesI $3 }
  | IMPLIESE COLON line_ref COMMA line_ref { ImpliesE ($3, $5) }

