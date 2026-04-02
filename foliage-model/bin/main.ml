open Js_of_ocaml
open Foliage_model

let () =
  Js.export "FoliageModel"
    (object%js
       method exampleProp =
         let open Syntax in
         let prop = Implies (Atom "P", Or (Atom "Q", Not (Atom "R"))) in
         Js.string (prop_to_string prop)
    end)
