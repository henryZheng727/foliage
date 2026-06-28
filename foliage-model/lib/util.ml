open Types

let rec prop_to_string = function
  | True -> "True"
  | False -> "False"
  | Atom s -> s
  | And (p, q) -> Printf.sprintf "(%s & %s)" (prop_to_string p) (prop_to_string q)
  | Or (p, q) -> Printf.sprintf "(%s + %s)" (prop_to_string p) (prop_to_string q)
  | Implies (p, q) -> Printf.sprintf "(%s -> %s)" (prop_to_string p) (prop_to_string q)

let prop_of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.token lexbuf

let line_ref_to_string = function
  | Single n -> string_of_int n
  | Range (a, b) -> Printf.sprintf "%d-%d" a b

let rule_to_string = function
  | Premise -> "Premise"
  | Assumption -> "Assumption"
  | Reit r -> Printf.sprintf "Reit: %s" (line_ref_to_string r)
  | AndI (r1, r2) -> Printf.sprintf "&Intro: %s, %s" (line_ref_to_string r1) (line_ref_to_string r2)
  | AndEL r -> Printf.sprintf "&Elim: %s" (line_ref_to_string r)
  | AndER r -> Printf.sprintf "&Elim: %s" (line_ref_to_string r)
  | OrIL r -> Printf.sprintf "+Intro: %s" (line_ref_to_string r)
  | OrIR r -> Printf.sprintf "+Intro: %s" (line_ref_to_string r)
  | OrE (r1, r2, r3) -> Printf.sprintf "+Elim: %s, %s, %s" (line_ref_to_string r1) (line_ref_to_string r2) (line_ref_to_string r3)
  | ImpliesI r -> Printf.sprintf "->Intro: %s" (line_ref_to_string r)
  | ImpliesE (r1, r2) -> Printf.sprintf "->Elim: %s, %s" (line_ref_to_string r1) (line_ref_to_string r2)
  | FalseE r -> Printf.sprintf "FalseE: %s" (line_ref_to_string r)

let statement_to_string stmt =
  Printf.sprintf "%s%d | %s by %s"
    (String.make (stmt.depth * 2) '|')
    stmt.line_num
    (prop_to_string stmt.formula)
    (rule_to_string stmt.rule)
