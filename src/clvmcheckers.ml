open Stringmap
open Dsl

let program =
  [ ( "maskFor"
    , { name = "maskFor"
      ; args = ["x"; "y"]
      ; body = "(lsh 1 (+ (* 8 y) x))"
      }
    )
  ] |> StringMapBuilder.build
