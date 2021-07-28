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
  [ ( "maskFor", ["pt"], Mask zero, "(lsh (- 0 (+ (* 8 (f pt)) (r pt))) 1)")
  ]
  |> List.map
    (fun (name,args,res,code) ->
       let opts = compileOpts name in
       let modcode = "(mod (" ^ (String.concat " " args) ^ ") " ^ code ^ ")" in
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
