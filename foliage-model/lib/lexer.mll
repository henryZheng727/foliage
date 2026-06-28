{
  open Parser
}

let whitespace = [' ' '\t' '\r']
let newline = '\n'
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '_')*
let number = digit+

rule token = parse
  | newline               { Lexing.new_line lexbuf; token lexbuf }
  | whitespace+           { token lexbuf }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '&'                   { AND }
  | '+'                   { OR }
  | "->"                  { IMPLIES }
  | '~'                   { NOT }
  | '|'                   { PIPE }
  | "by"                  { BY }
  | "True"                { TRUE }
  | "False"               { FALSE }
  | "Premise"             { PREMISE }
  | "Assumption"          { ASSUMPTION }
  | "Reit"                { REIT }
  | "&Intro"              { ANDI }
  | "&Elim"               { ANDEL }
  | "+Intro"              { ORI }
  | "+Elim"               { ORE }
  | "->Intro"             { IMPLIESI }
  | "->Elim"              { IMPLIESE }
  | ':'                   { COLON }
  | ','                   { COMMA }
  | '-'                   { DASH }
  | number as n           { NUMBER (int_of_string n) }
  | ident as s            { ATOM s }
  | eof                   { EOF }
  | _                     { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }

