open Msg
open Tea.App
open Tea.Html

open Checktypes
open Checkmethods
open Game

let init _ = Game.init ()

let update game = function
  | Noop -> game
  | Click (x,y) -> Game.click x y game

let rec range l h =
  if l >= h then [] else l :: (range (l+1) h)

let squareColor x y =
  if (x + y) mod 2 = 0 then
    "dark"
  else
    "light"

let view game =
  let b = game.board in
  let moveTo =
    game.allowedMoves
    |> CheckersMoveSet.elements
    |> List.map (fun m -> (m.toX, m.toY))
    |> PointSet.of_list
  in
  let moveFrom =
    game.allowedMoves
    |> CheckersMoveSet.elements
    |> List.map (fun m -> (m.fromX, m.fromY))
    |> PointSet.of_list
  in
  let checkersRow y x =
    let sq = squareColor x y in
    let highlight =
      match game.selectedChecker with
      | Some (sx,sy) -> PointSet.add (sx,sy) moveTo
      | None -> moveFrom
    in
    let squareClass = "square-" ^ sq in
    let innerPart i =
      if PointSet.mem (x,y) highlight then
        div [class' "square-highlight" ; onClick (Click (x,y))] [i]
      else
        i
    in
    let ch = checkerAt x y b in
    ch
    |> Option.map
      (fun p ->
         let pieceClass = "piece-" ^ (colorToString (checkerColor p)) in
         let kingClass =
           if isKing p then "king" else "pawn"
         in
         div [class' squareClass]
           [ innerPart (div [class' (pieceClass ^ " " ^ kingClass)] [])
           ]
      )
    |> Xoption.default (div [class' squareClass] [ innerPart (div [] []) ])
  in
  let board =
    range 0 8
    |> List.map
      (fun y ->
         div [class' "checkers-row"]
           (range 0 8 |> List.map (checkersRow y))
      )
  in
  let heading =
    Game.win game
    |> Option.map
      (fun winner -> div [] [ text "Winner " ; text @@ colorToString winner ])
    |> Xoption.default
      (div [] [ text "Next Move: " ; text @@ colorToString b.next ])
  in
  div []
    [ heading
    ; div [class' "checkers-board"] board
    ]

let main =
  beginnerProgram {
    model = init ();
    update;
    view;
  }
