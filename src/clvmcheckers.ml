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
    ; ( "makeKing", ["color"], Checker (King Red), "(c 1 color)")
    ; ( "makePawn", ["color"], Checker (King Red), "(c 0 color)")
    ; ( "checkerAt1", ["mask"; "(next king red black)"], Checker (King Red),
        "(if (logand mask red) (list (if (logand mask king) (makeKing 0) (makePawn 0))) (if (logand mask black) (list (if (logand mask king) (makeKing 1) (makePawn 1))) (quote ())))"
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
        "(= (f checker) 1)"
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
    ; ( "abs", ["s"], MaxSteps 0,
        "(if (> s 0) s (- 0 s))"
      )
    ; ( "direction1", ["fromX"; "fromY"; "toX"; "toY"], Point (0,0),
        "(c (- toX fromX) (- toY fromY))"
      )
    ; ( "direction", ["m"], Point (0,0),
        "(direction1 (f (f m)) (r (f m)) (f (r m)) (r (r m)))"
      )
    ; ( "validDiagonal3", ["rr"], MaxSteps 0,
        "(= (f rr) (r rr))"
      )
    ; ( "validDiagonal2", ["m"], MaxSteps 0,
        "(validDiagonal3 (direction m))"
      )
    ; ( "validDiagonal1", ["m" ; "fromX" ; "fromY" ; "toX" ; "toY"], MaxSteps 0,
        "(if (+ (= fromX toX) (= fromY toY)) () (validDiagonal2 m))"
      )
    ; ( "validDiagonal", ["m"], MaxSteps 0,
        "(validDiagonal1 m (f (f m)) (r (f m)) (f (r m)) (r (r m)))"
      )
    ; ( "checkerColor", ["ch"], Color Red,
        "(r ch)"
      )
    ; ( "otherColor", ["color"], Color Red,
        "(if (= color 0) 1 0)"
      )
(*
    ; ( "board$next", ["b"], Color Red,
        "(f b)"
      )
    ; ( "board$king", ["b"], Mask zero,
        "(f (r b))"
      )
    ; ( "board$red", ["b"], Mask zero,
        "(f (r (r b)))"
      )
    ; ( "board$black", ["b"], Mask zero,
        "(f (r (r (r b))))"
      )
    ; ( "addChecker2", ["b"; "king"; "red"; "black"], Board emptyBoard,
        "(list (board$next b) (logor king (board$king b)) (logor red (board$red b)) (logor black (board$black b)))"
      )
    ; ( "addChecker1", ["b"; "mask"; "c"; 
    ; ( "addChecker", ["pt"; "c"; "b"], Board emptyBoard,
        "(addChecker1 b ("
   *)
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
