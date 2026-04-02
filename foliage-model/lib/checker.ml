let parse (s : string) : Syntax.prop =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.token lexbuf