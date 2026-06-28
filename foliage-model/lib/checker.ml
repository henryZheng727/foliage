let parse (s : string) : Types.prop =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.token lexbuf

let rec check (p : Types.prop) : bool =
  match p with
  | Types.True -> true
  | Types.False -> false
  | Types.Atom _ -> true
  | Types.And (a, b) -> check a && check b
  | Types.Or (a, b) -> check a || check b
  | Types.Implies (a, b) -> (not (check a)) || check b
