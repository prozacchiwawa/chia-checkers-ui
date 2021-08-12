open Checktypes
open Clvmcheckers
open Dsl

let checkerColor = function
  | King c -> c
  | Pawn c -> c

let otherColor = function
  | Red -> Black
  | _ -> Red

let isKing = function
  | King _ -> true
  | _ -> false

let checkerAt x y b =
  match exec program "checkerAt" [Point (x,y); Board b] with
  | Maybe (AJust (Checker c)) -> Some c
  | Maybe ANothing -> None

let move m b =
  match exec program "move" [Move m; Board b] with
  | Board b -> b

let availableMoves b =
  match exec program "availableMoves" [Board b] with
  | AList ms ->
    List.map
      (function
        | Move m -> m
      )
      ms

let availableMovesForChecker ch x y b =
  match exec program "availableMovesForChecker" [Checker ch; Point (x,y); Board b] with
  | AList ps ->
    List.map
      (function
        | Point p -> p
      )
      ps

let colorToString = function
  | Red -> "red"
  | Black -> "black"

let standardBoard =
  { next = Black
  ; king = zero
  ; red = BigInteger.bigIntBaseN (`String "a040a040a040a040") (`Int 16)
  ; black = BigInteger.bigIntBaseN (`String "0205020502050205") (`Int 16)
  }
