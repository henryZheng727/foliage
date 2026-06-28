open Js_of_ocaml
open Foliage_model
open Types

let () =
  Js.export "FoliageModel"
    (object%js
       method exampleProp =
         let prop = Implies (Atom "P", Or (Atom "Q", Implies (Atom "R", False))) in
         Js.string (Util.prop_to_string prop)
    end)
