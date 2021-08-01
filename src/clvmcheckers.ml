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
    [ ( "label", ["_"; "actually"], MaxSteps 0, "actually")
    ; ( "maskFor", ["pt"], Mask zero, "(lsh 1 (+ (* 8 (f pt)) (r pt)))")
    ; ( "makeKing", ["color"], Checker (King Red), "(c 1 color)")
    ; ( "makePawn", ["color"], Checker (King Red), "(c 0 color)")
    ; ( "m$fromX", ["m"], MaxSteps 0,
        "(f (f m))"
      )
    ; ( "m$fromY", ["m"], MaxSteps 0,
        "(r (f m))"
      )
    ; ( "m$toX", ["m"], MaxSteps 0,
        "(f (r m))"
      )
    ; ( "m$toY", ["m"], MaxSteps 0,
        "(r (r m))"
      )
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
    ; ( "manhattanDistance", ["m"], MaxSteps 0,
        "(abs (- (m$fromX m) (m$toX m)))"
      )
    ; ( "abs", ["s"], MaxSteps 0,
        "(if (> s 0) s (- 0 s))"
      )
    ; ( "direction1", ["fromX"; "fromY"; "toX"; "toY"], Point (0,0),
        "(c (- toX fromX) (- toY fromY))"
      )
    ; ( "direction", ["m"], Point (0,0),
        "(c (- (m$toX m) (m$fromX m)) (- (m$toY m) (m$fromY m)))"
      )
    ; ( "validDiagonal3", ["dir"], MaxSteps 0,
        "(= (abs (f dir)) (abs (r dir)))"
      )
    ; ( "validDiagonal2", ["m"], MaxSteps 0,
        "(validDiagonal3 (direction m))"
      )
    ; ( "validDiagonal1", ["m"], MaxSteps 0,
        "(if (+ (= (m$fromX m) (m$toX m)) (= (m$fromY m) (m$toY m))) () (validDiagonal2 m))"
      )
    ; ( "validDiagonal", ["m"], MaxSteps 0,
        "(validDiagonal1 m)"
      )
    ; ( "checkerColor", ["ch"], Color Red,
        "(r ch)"
      )
    ; ( "otherColor", ["color"], Color Red,
        "(if (= color 0) 1 0)"
      )
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
    ; ( "addChecker2", ["king"; "red"; "black"; "b"], Board emptyBoard,
        "(list (board$next b) (logior king (board$king b)) (logior red (board$red b)) (logior black (board$black b)))"
      )
    ; ( "addChecker1", ["mask"; "ch"; "b"], Board emptyBoard,
        "(addChecker2 (if (isKing ch) mask 0) (if (checkerColor ch) 0 mask) (if (checkerColor ch) mask 0) b)"
      )
    ; ( "addChecker", ["pt"; "ch"; "b"], Board emptyBoard,
        "(addChecker1 (maskFor pt) ch b)"
      )
    ; ( "fromJust", ["mObj"], MaxSteps 0,
        "(f mObj)"
      )
    ; ( "just", ["obj"], MaxSteps 0,
        "(list obj)"
      )
    ; ( "colorOfMaybeChecker", ["mCh"], Maybe (AJust (Color Red)),
        "(if mCh (just (checkerColor (fromJust mCh))) ())"
      )
    ; ( "jumpState$sEqSteps", ["js"], MaxSteps 0,
        "(f js)"
      )
    ; ( "jumpState$sMod2Eq0", ["js"], MaxSteps 0,
        "(f (r js))"
      )
    ; ( "jumpState$theChecker", ["js"], Maybe (AJust (Checker (King Red))),
        "(f (r (r js)))"
      )
    ; ( "jumpState$otherColor", ["js"], Color Red,
        "(f (r (r (r js))))"
      )
    ; ( "jumpAtCoords0", ["fromX"; "fromY"; "dx"; "dy"; "steps"; "s"], Point (0,0),
        "(c (+ fromX (* s (/ dx steps))) (+ fromY (* s (/ dy steps))))"
      )
    ; ( "jumpAtCoords", ["fromX"; "fromY"; "dir"; "steps"; "s"], Point (0,0),
        "(jumpAtCoords0 fromX fromY (f dir) (r dir) steps s)"
      )
    ; ( "newJumpState2", ["oc"; "sEqSteps"; "sMod2Eq0"; "theChecker"], MaxSteps 0,
        "(list sEqSteps sMod2Eq0 theChecker (= (colorOfMaybeChecker theChecker) (just oc)))"
      )
    ; ( "newJumpState1", ["oc"; "steps"; "jcoord"; "b"; "s"], MaxSteps 0,
        "(newJumpState2 oc (= steps s) (not (r (divmod s 2))) (checkerAt jcoord b))"
      )
    ; ( "newJumpState", ["oc"; "steps"; "m"; "b"; "s"], MaxSteps 0,
        "(newJumpState1 oc steps (jumpAtCoords (m$fromX m) (m$fromY m) (direction m) steps s) b s)"
      )
    ; ( "jumpsNextStep", ["steps"; "color"; "m"; "b"; "a"; "s"; "js"], Maybe (AJust (AList [Point (0,0)])),
        "(if (label \"true true None _\" (* (* (jumpState$sEqSteps js) (jumpState$sMod2Eq0 js)) (not (jumpState$theChecker js)))) (just a) (if (label \"_ true Some _\" (* (jumpState$sMod2Eq0 js) (not (not (jumpState$theChecker js))))) () (if (label \"_ false _ true\" (* (not (jumpState$sMod2Eq0 js)) (jumpState$otherColor js))) (nextJump1 steps color m b (c (jumpAtCoords (m$fromX m) (m$fromY m) (direction m) steps s) a) (+ s 1)) (if (label \"_ true None _\" (* (not (jumpState$sMod2Eq0 js)) (jumpState$otherColor js))) (nextJump1 steps color m b a (+ s 1)) ()))))"
      )
    ; ( "nextJump1", ["steps"; "color"; "m"; "b"; "a"; "s"], Maybe (AJust (AList [Point (0,0)])),
        "(jumpsNextStep steps color m b a s (newJumpState (otherColor color) steps m b s))"
      )
    ; ( "jumps", ["color"; "m"; "b"], Maybe (AJust (AList [Point (0,0)])),
        "(nextJump1 (manhattanDistance m) color m b () 1)"
      )
    ; ( "forward", ["color"; "dy"], MaxSteps 0,
        "(i color (> dy 0) (> 0 dy))"
      )
    ; ( "kingRow", ["color"], MaxSteps 0,
        "(i color 7 0)"
      )
    ; ( "nextMove", ["b"], Board emptyBoard,
        "(list (otherColor (board$next b)) (board$king b) (board$red b) (board$black b))"
      )
    ; ( "availableJumps2", ["a"; "s"; "color"; "dx"; "dy"; "x"; "y"; "b"; "atX"; "atY"; "jlist"], AList [Point (0,0)],
        "(availableJumps (if jlist (c (c atX atY) a) a) (+ s 2) color dx dy x y b)"
      )
    ; ( "availableJumps1", ["a"; "s"; "color"; "dx"; "dy"; "x"; "y"; "b"; "atX"; "atY"], AList [Point (0,0)],
        "(if (inBounds (c atX atY)) (availableJumps2 a s color dx dy x y b atX atY (jumps color (c (c x y) (c atX atY)) b)) a)"
      )
    ; ( "availableJumps", ["a"; "s"; "color"; "dx"; "dy"; "x"; "y"; "b"], AList [Point (0,0)],
        "(availableJumps1 a s color dx dy x y b (+ x (* s dx)) (+ y (* s dy)))"
      )
    ; ( "moddiv1", ["res"], MaxSteps 0,
        "(c (r res) (f res))"
      )
    ; ( "moddiv", ["n";"d"], MaxSteps 0,
        "(moddiv1 (divmod n d))"
      )
    ; ( "listCheckersWithColor2", ["head"; "rest"], AList [APair (Point (0,0), Checker (King Red))],
        "(if head (c head rest) rest)"
      )
    ; ( "listCheckersWithColor1", ["n"; "color"; "b"; "chq"], AList [APair (Point (0,0), Checker (King Red))],
        "(listCheckersWithColor2 (if chq (if (= (colorOfMaybeChecker chq) (just color)) (c (moddiv n 8) (fromJust chq)) ()) ()) (listCheckersWithColor (+ n 1) color b))"
      )
    ; ( "listCheckersWithColor", ["n"; "color"; "b"], AList [APair (Point (0,0), Checker (King Red))],
        "(if (> n 63) () (listCheckersWithColor1 n color b (checkerAt (moddiv n 8) b)))"
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
