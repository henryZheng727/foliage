type prop =
  | True
  | False
  | Atom of string
  | And of prop * prop
  | Or of prop * prop
  | Implies of prop * prop

type line_ref =
  | Single of int
  | Range of int * int

type rule =
  | Premise
  | Assumption
  | Reit of line_ref
  | AndI of line_ref * line_ref
  | AndEL of line_ref
  | AndER of line_ref
  | OrIL of line_ref
  | OrIR of line_ref
  | OrE of line_ref * line_ref * line_ref
  | ImpliesI of line_ref
  | ImpliesE of line_ref * line_ref
  | FalseE of line_ref

type statement = {
  line_num: int;
  formula: prop;
  depth: int;
  rule: rule;
}
