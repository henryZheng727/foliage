open Foliage_model.Types
open Foliage_model.Util

(* Helper to compare props *)
let assert_equal_prop expected actual msg =
  if expected <> actual then
    Printf.printf "FAIL: %s\n  Expected: %s\n  Got: %s\n" msg
      (prop_to_string expected) (prop_to_string actual)

(* Helper to parse formula strings *)
let parse_formula s =
  let lexbuf = Lexing.from_string s in
  Foliage_model.Parser.main Foliage_model.Lexer.token lexbuf

(* ============================================================================ *)
(* LEXER TESTS *)
(* ============================================================================ *)

let test_lexer_atoms () =
  print_endline "Testing lexer: atoms";
  let props =
    [ ("A", Atom "A"); ("P", Atom "P"); ("Proposition", Atom "Proposition") ]
  in
  List.iter
    (fun (s, expected) ->
      let parsed = parse_formula s in
      assert_equal_prop expected parsed s)
    props

let test_lexer_constants () =
  print_endline "Testing lexer: constants";
  let props = [ ("True", True); ("False", False) ] in
  List.iter
    (fun (s, expected) ->
      let parsed = parse_formula s in
      assert_equal_prop expected parsed s)
    props

let test_lexer_operators () =
  print_endline "Testing lexer: operators";
  let props =
    [
      ("A & B", And (Atom "A", Atom "B"));
      ("P + Q", Or (Atom "P", Atom "Q"));
      ("X -> Y", Implies (Atom "X", Atom "Y"));
      ("~A", Implies (Atom "A", False));
    ]
  in
  List.iter
    (fun (s, expected) ->
      let parsed = parse_formula s in
      assert_equal_prop expected parsed s)
    props

(* ============================================================================ *)
(* PARSER TESTS - PRECEDENCE *)
(* ============================================================================ *)

let test_parser_precedence () =
  print_endline "Testing parser: operator precedence";

  (* NOT has highest precedence *)
  let p1 = parse_formula "~A & B" in
  assert_equal_prop
    (And (Implies (Atom "A", False), Atom "B"))
    p1 "NOT precedence";

  (* AND has higher precedence than OR *)
  let p2 = parse_formula "A + B & C" in
  assert_equal_prop
    (Or (Atom "A", And (Atom "B", Atom "C")))
    p2 "AND vs OR precedence";

  (* OR has higher precedence than IMPLIES *)
  let p3 = parse_formula "A -> B + C" in
  assert_equal_prop
    (Implies (Atom "A", Or (Atom "B", Atom "C")))
    p3 "OR vs IMPLIES precedence";

  (* IMPLIES is right-associative *)
  let p4 = parse_formula "A -> B -> C" in
  assert_equal_prop
    (Implies (Atom "A", Implies (Atom "B", Atom "C")))
    p4 "IMPLIES right-associativity";

  (* AND is left-associative *)
  let p5 = parse_formula "A & B & C" in
  assert_equal_prop
    (And (And (Atom "A", Atom "B"), Atom "C"))
    p5 "AND left-associativity";

  (* OR is left-associative *)
  let p6 = parse_formula "A + B + C" in
  assert_equal_prop
    (Or (Or (Atom "A", Atom "B"), Atom "C"))
    p6 "OR left-associativity"

let test_parser_parentheses () =
  print_endline "Testing parser: parentheses";

  (* Parentheses override precedence *)
  let p1 = parse_formula "(A + B) & C" in
  assert_equal_prop
    (And (Or (Atom "A", Atom "B"), Atom "C"))
    p1 "Parentheses override AND/OR";

  let p2 = parse_formula "A -> (B + C)" in
  assert_equal_prop
    (Implies (Atom "A", Or (Atom "B", Atom "C")))
    p2 "Nested parentheses";

  let p3 = parse_formula "(A -> B) -> C" in
  assert_equal_prop
    (Implies (Implies (Atom "A", Atom "B"), Atom "C"))
    p3 "Parentheses left-associate IMPLIES"

let test_parser_complex () =
  print_endline "Testing parser: complex formulas";

  (* From examples *)
  let p1 = parse_formula "(A & B) & C" in
  assert_equal_prop
    (And (And (Atom "A", Atom "B"), Atom "C"))
    p1 "Conjunction associative";

  let p2 = parse_formula "A -> B" in
  assert_equal_prop (Implies (Atom "A", Atom "B")) p2 "Simple implication";

  let p3 = parse_formula "~B -> ~A" in
  assert_equal_prop
    (Implies (Implies (Atom "B", False), Implies (Atom "A", False)))
    p3 "Contrapositive";

  let p4 = parse_formula "~~A" in
  assert_equal_prop
    (Implies (Implies (Atom "A", False), False))
    p4 "Double negation";

  let p5 = parse_formula "A + ~A" in
  assert_equal_prop
    (Or (Atom "A", Implies (Atom "A", False)))
    p5 "Excluded middle"

(* ============================================================================ *)
(* UTILITY FUNCTION TESTS *)
(* ============================================================================ *)

let test_prop_to_string () =
  print_endline "Testing prop_to_string";

  let cases =
    [
      (True, "True");
      (False, "False");
      (Atom "A", "A");
      (And (Atom "A", Atom "B"), "(A & B)");
      (Or (Atom "A", Atom "B"), "(A + B)");
      (Implies (Atom "A", Atom "B"), "(A -> B)");
      (Implies (Atom "A", False), "(A -> False)");
      (And (And (Atom "A", Atom "B"), Atom "C"), "((A & B) & C)");
      (Or (Atom "A", And (Atom "B", Atom "C")), "(A + (B & C))");
    ]
  in
  List.iter
    (fun (prop, expected) ->
      let result = prop_to_string prop in
      if result <> expected then
        Printf.printf "FAIL: prop_to_string\n  Expected: %s\n  Got: %s\n"
          expected result)
    cases

let test_line_ref_to_string () =
  print_endline "Testing line_ref_to_string";

  let cases =
    [
      (Single 1, "1");
      (Single 10, "10");
      (Range (1, 3), "1-3");
      (Range (5, 12), "5-12");
    ]
  in
  List.iter
    (fun (ref, expected) ->
      let result = line_ref_to_string ref in
      if result <> expected then
        Printf.printf "FAIL: line_ref_to_string\n  Expected: %s\n  Got: %s\n"
          expected result)
    cases

let test_rule_to_string () =
  print_endline "Testing rule_to_string";

  let cases =
    [
      (Premise, "Premise");
      (Assumption, "Assumption");
      (Reit (Single 1), "Reit: 1");
      (Reit (Range (1, 3)), "Reit: 1-3");
      (AndI (Single 1, Single 2), "&Intro: 1, 2");
      (AndEL (Single 3), "&Elim: 3");
      (AndER (Single 3), "&Elim: 3");
      (OrIL (Single 1), "+Intro: 1");
      (OrIR (Single 2), "+Intro: 2");
      (ImpliesI (Range (2, 5)), "->Intro: 2-5");
      (ImpliesE (Single 1, Single 2), "->Elim: 1, 2");
      (FalseE (Single 4), "FalseE: 4");
    ]
  in
  List.iter
    (fun (rule, expected) ->
      let result = rule_to_string rule in
      if result <> expected then
        Printf.printf "FAIL: rule_to_string\n  Expected: %s\n  Got: %s\n"
          expected result)
    cases

let test_statement_to_string () =
  print_endline "Testing statement_to_string";

  let stmt1 =
    {
      line_num = 1;
      formula = Implies (Atom "A", Atom "B");
      depth = 0;
      rule = Premise;
    }
  in
  let result1 = statement_to_string stmt1 in
  if not (String.contains result1 '1') then
    Printf.printf "FAIL: statement_to_string missing line number\n";

  let stmt2 =
    { line_num = 2; formula = Atom "A"; depth = 1; rule = Assumption }
  in
  let result2 = statement_to_string stmt2 in
  if not (String.contains result2 '|') then
    Printf.printf "FAIL: statement_to_string missing depth markers\n"

(* ============================================================================ *)
(* ROUNDTRIP TESTS *)
(* ============================================================================ *)

let test_roundtrip () =
  print_endline "Testing parse -> to_string roundtrip";

  let formulas =
    [
      "A";
      "True";
      "False";
      "A & B";
      "A + B";
      "A -> B";
      "~A";
      "A & B & C";
      "A + B + C";
      "A -> B -> C";
      "(A & B) + C";
      "A + (B & C)";
      "(A + B) -> C";
      "A -> (B & C)";
    ]
  in

  List.iter
    (fun formula ->
      try
        let parsed = parse_formula formula in
        let stringified = prop_to_string parsed in
        let reparsed = parse_formula stringified in
        if parsed <> reparsed then
          Printf.printf "FAIL: Roundtrip failed for %s\n  Stringified: %s\n"
            formula stringified
      with e ->
        Printf.printf "FAIL: Exception parsing %s: %s\n" formula
          (Printexc.to_string e))
    formulas

(* ============================================================================ *)
(* EDGE CASES *)
(* ============================================================================ *)

let test_edge_cases () =
  print_endline "Testing edge cases";

  (* Multi-character atoms *)
  let p1 = parse_formula "Proposition & Question" in
  assert_equal_prop
    (And (Atom "Proposition", Atom "Question"))
    p1 "Multi-char atoms";

  (* Atoms with numbers and underscores *)
  let p2 = parse_formula "A_1 & B2" in
  assert_equal_prop
    (And (Atom "A_1", Atom "B2"))
    p2 "Atoms with numbers and underscores";

  (* Multiple levels of parentheses *)
  let p3 = parse_formula "(((A)))" in
  assert_equal_prop (Atom "A") p3 "Multiple parentheses levels";

  (* Mix of all operators *)
  let p4 = parse_formula "~(A & B) -> (C + D)" in
  assert_equal_prop
    (Implies (Implies (And (Atom "A", Atom "B"), False), Or (Atom "C", Atom "D")))
    p4 "Mix of all operators"

let test_types_equality () =
  print_endline "Testing types equality";

  (* Props are comparable *)
  let p1 = Atom "A" in
  let p2 = Atom "A" in
  let p3 = Atom "B" in
  if p1 <> p2 then Printf.printf "FAIL: Equal atoms should be equal\n";
  if p1 = p3 then Printf.printf "FAIL: Different atoms should not be equal\n";

  (* Complex props *)
  let c1 = And (Atom "A", Or (Atom "B", Atom "C")) in
  let c2 = And (Atom "A", Or (Atom "B", Atom "C")) in
  let c3 = And (Atom "B", Or (Atom "A", Atom "C")) in
  if c1 <> c2 then Printf.printf "FAIL: Equal complex props should be equal\n";
  if c1 = c3 then
    Printf.printf "FAIL: Different complex props should not be equal\n"

(* ============================================================================ *)
(* LINE REFERENCE TESTS *)
(* ============================================================================ *)

let test_line_references () =
  print_endline "Testing line references";

  let single1 = Single 1 in
  let single2 = Single 1 in
  let single3 = Single 2 in
  let range1 = Range (1, 3) in
  let range2 = Range (1, 3) in
  let range3 = Range (1, 4) in

  if single1 <> single2 then
    Printf.printf "FAIL: Equal single refs should be equal\n";
  if single1 = single3 then
    Printf.printf "FAIL: Different single refs should not be equal\n";
  if range1 <> range2 then
    Printf.printf "FAIL: Equal range refs should be equal\n";
  if range1 = range3 then
    Printf.printf "FAIL: Different range refs should not be equal\n"

(* ============================================================================ *)
(* RULE PATTERN TESTS *)
(* ============================================================================ *)

let test_rule_patterns () =
  print_endline "Testing rule patterns";

  let rules =
    [
      Premise;
      Assumption;
      Reit (Single 1);
      Reit (Range (1, 5));
      AndI (Single 1, Single 2);
      AndI (Range (1, 3), Single 4);
      AndEL (Single 1);
      AndER (Single 2);
      OrIL (Single 1);
      OrIR (Single 2);
      OrE (Single 1, Single 2, Single 3);
      ImpliesI (Range (1, 5));
      ImpliesE (Single 1, Single 2);
      FalseE (Single 3);
    ]
  in

  List.iter
    (fun rule ->
      let s = rule_to_string rule in
      if String.length s = 0 then
        Printf.printf "FAIL: rule_to_string returned empty string\n")
    rules

(* ============================================================================ *)
(* FORMULA STRUCTURES *)
(* ============================================================================ *)

let test_formula_structures () =
  print_endline "Testing various formula structures";

  (* Deeply nested AND *)
  let deep_and = And (And (And (Atom "A", Atom "B"), Atom "C"), Atom "D") in
  let str_and = prop_to_string deep_and in
  if String.length str_and < 10 then
    Printf.printf "FAIL: Deep AND stringification too short\n";

  (* Deeply nested OR *)
  let deep_or = Or (Or (Or (Atom "A", Atom "B"), Atom "C"), Atom "D") in
  let str_or = prop_to_string deep_or in
  if String.length str_or < 10 then
    Printf.printf "FAIL: Deep OR stringification too short\n";

  (* Nested implications *)
  let nested_impl =
    Implies (Implies (Atom "A", Atom "B"), Implies (Atom "C", Atom "D"))
  in
  let str_impl = prop_to_string nested_impl in
  if String.length str_impl < 10 then
    Printf.printf "FAIL: Nested IMPLIES stringification too short\n";

  (* Mixed nesting *)
  let mixed = And (Or (Atom "A", Atom "B"), Implies (Atom "C", Atom "D")) in
  let str_mixed = prop_to_string mixed in
  if String.length str_mixed < 10 then
    Printf.printf "FAIL: Mixed structure stringification too short\n"

(* ============================================================================ *)
(* STATEMENT TESTS *)
(* ============================================================================ *)

let test_statement_creation () =
  print_endline "Testing statement creation";

  let stmts =
    [
      { line_num = 1; formula = Atom "A"; depth = 0; rule = Premise };
      { line_num = 2; formula = Atom "B"; depth = 0; rule = Assumption };
      {
        line_num = 3;
        formula = And (Atom "A", Atom "B");
        depth = 1;
        rule = AndI (Single 1, Single 2);
      };
      { line_num = 4; formula = Atom "A"; depth = 2; rule = Reit (Single 1) };
    ]
  in

  List.iter
    (fun stmt ->
      let s = statement_to_string stmt in
      if not (String.contains s '|') then
        Printf.printf "FAIL: Statement at depth %d missing pipe symbol\n"
          stmt.depth)
    stmts

(* ============================================================================ *)
(* MAIN TEST RUNNER *)
(* ============================================================================ *)

let () =
  print_endline "\n========== FOLIAGE MODEL TESTS ==========\n";

  (* Lexer tests *)
  test_lexer_atoms ();
  test_lexer_constants ();
  test_lexer_operators ();

  (* Parser tests *)
  test_parser_precedence ();
  test_parser_parentheses ();
  test_parser_complex ();

  (* Utility function tests *)
  test_prop_to_string ();
  test_line_ref_to_string ();
  test_rule_to_string ();
  test_statement_to_string ();

  (* Edge case tests *)
  test_edge_cases ();
  test_types_equality ();
  test_line_references ();
  test_rule_patterns ();
  test_formula_structures ();
  test_statement_creation ();

  (* Roundtrip tests *)
  test_roundtrip ();

  print_endline "\n========== ALL TESTS COMPLETED ==========\n"
