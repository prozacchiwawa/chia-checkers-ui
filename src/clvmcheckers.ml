open Comptypes
open Codegen

open Checktypes
open Stringmap
open Dsl

exception CompileFailure of string

let compileOpts name =
  { includeDirs = []
  ; filename = name
  ; assemble = false
  ; stdenv = true
  ; inDefun = false
  ; startEnv = None
  ; compiler = None
  ; readNewFile =
      (fun opts prevname filename ->
         if filename == "*macros*" then
           CompileOk (filename, String.concat "\n" Macros.macros)
         else
           CompileError (Srcloc.start name, "no such include " ^ filename)
      )

  ; compileProgram =
      (fun opts program ->
         Frontend.frontend opts [program]
         |> compBind (fun m -> codegen opts m)
      )
  }

let program =
  let sources =
    [ ( "maskFor", ["pt"], Mask zero, "(lsh 1 (+ (* 8 (f pt)) (r pt)))")
    ; ( "makeKing", ["color"], Checker (King Red), "(c \"king\" color)")
    ; ( "makePawn", ["color"], Checker (King Red), "(c \"pawn\" color)")
    ; ( "checkerAt1", ["mask"; "(next king red black)"], Checker (King Red),
        "(if (logand mask red) (list (if (logand mask king) (makeKing \"red\") (makePawn \"red\"))) (if (logand mask black) (list (if (logand mask king) (makeKing \"black\") (makePawn \"black\"))) (quote ())))"
      )
    ; ( "checkerAt", ["pt"; "b"], Maybe (AJust (Checker (King Red))),
        "(checkerAt1 (maskFor pt) b)"
      )
    ; ( "removeChecker1", ["mask"; "(next king red black)"], Board emptyBoard,
        "(list next (logxor king (if (logand mask king) mask 0)) (logxor red (if (logand mask red) mask 0)) (logxor black (if (logand mask black) mask 0)))"
      )
    ; ( "removeChecker", ["pt"; "b"], Board emptyBoard,
        "(removeChecker1 (maskFor pt) b)"
      )
    ; ( "isKing", ["checker"], MaxSteps 0,
        "(= (f checker) \"king\")"
      )
    ; ( "inBounds1", ["x"; "y"], MaxSteps 0,
        "(* (* (+ (> x 0) (= x 0)) (> 8 x)) (* (+ (> y 0) (= y 0)) (> 8 y)))"
      )
    ; ( "inBounds", ["pt"], MaxSteps 0,
        "(inBounds1 (f pt) (r pt))"
      )
    ; ( "manhattanDistance1", ["fromX"; "toX"], MaxSteps 0,
        "(if (> fromX toX) (- fromX toX) (- toX fromX))"
      )
    ; ( "manhattanDistance", ["m"], MaxSteps 0,
        "(manhattanDistance1 (f (f m)) (f (r m)))"
      )
    ]
  in
  let progbody =
    sources
    |> List.map
      (fun (name,args,res,code) ->
         "(defun " ^ name ^ " (" ^ (String.concat " " args) ^ ") " ^ code ^ ")"
      )
    |> String.concat "\n"
  in
  sources
  |> List.map
    (fun (name,args,res,code) ->
       let opts = compileOpts name in
       let modcode =
         "(mod (" ^ (String.concat " " args) ^ ") " ^ progbody ^ " (" ^ name ^ " " ^ (String.concat " " args) ^ "))"
       in
       let _ = Js.log modcode in
       match Compiler.compile_file opts modcode with
       | CompileOk v ->
         (name
         , { name = name
           ; args = args
           ; result = res
           ; code = v
           }
         )
       | CompileError (at,e) ->
         raise (CompileFailure (Srcloc.toString at ^ ": " ^ e))
    )
  |> StringMapBuilder.build
