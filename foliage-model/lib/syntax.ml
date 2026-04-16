(* Propositions *)
type prop =
        | Atom of string
        | And of prop * prop
        | Or of prop * prop
        | Implies of prop * prop
        | Not of prop
        | True
        | False

let rec prop_to_string = function
  | Atom s -> s
  | And (a, b) -> "(" ^ prop_to_string a ^ " ∧ " ^ prop_to_string b ^ ")"
  | Or (a, b) -> "(" ^ prop_to_string a ^ " ∨ " ^ prop_to_string b ^ ")"
  | Implies (a, b) -> "(" ^ prop_to_string a ^ " → " ^ prop_to_string b ^ ")"
  | Not a -> "¬" ^ prop_to_string a
  | False -> "⊥"
