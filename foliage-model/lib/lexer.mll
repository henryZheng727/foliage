{
  open Parser
}

let whitespace = [' ' '\t' '\n' '\r']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | ['0'-'9' '_'])*

rule token = parse
  | whitespace+   { token lexbuf }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | "/\\"         { AND }
  | "\\/"         { OR }
  | "->"          { IMPLIES }
  | '~'           { NOT }
  | "False"       { FALSE }
  | ident as s    { ATOM s }
  | eof           { EOF }
