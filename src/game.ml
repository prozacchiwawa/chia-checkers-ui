open Checktypes
open Checkmethods

type gameInProgress =
  { selectedChecker : (int * int) option
  ; allowedMoves : CheckersMoveSet.t
  ; board : checkersBoard
  }

let init () =
  let board = standardBoard in
  let allowed =
    availableMoves board
    |> CheckersMoveSet.of_list
  in
  { selectedChecker = None
  ; allowedMoves = allowed
  ; board = board
  }

let win game =
  if CheckersMoveSet.cardinal game.allowedMoves = 0 then
    Some (otherColor game.board.next)
  else
    None

let click x y game =
  let _ = Js.log "click" in
  let _ = Js.log (x,y) in
  let _ = Js.log @@ Array.of_list @@ CheckersMoveSet.elements game.allowedMoves in
  match game.selectedChecker with
  | None ->
    let moveTo =
      match checkerAt x y game.board with
      | Some ch ->
        availableMovesForChecker ch x y game.board
      | None -> []
    in
    let canMove =
      moveTo
      |> List.map
        (fun (ex,ey) -> { fromX = x ; fromY = y ; toX = ex ; toY = ey })
      |> CheckersMoveSet.of_list
    in
    { game with
      selectedChecker = Some (x,y)
    ; allowedMoves = canMove
    }
  | Some (bx,by) ->
    let m = { fromX = bx ; fromY = by ; toX = x ; toY = y } in
    let _ = Js.log "Want move" in
    let _ = Js.log m in
    let _ = Js.log @@ Array.of_list @@ CheckersMoveSet.elements game.allowedMoves in
    let afterRejectMoves =
      availableMoves game.board
      |> CheckersMoveSet.of_list
    in
    let rejectGame =
      { game with
        selectedChecker = None
      ; allowedMoves = afterRejectMoves
      }
    in
    if CheckersMoveSet.mem m game.allowedMoves then
      let newBoard = move m game.board in
      let newMoves =
        availableMoves newBoard
        |> CheckersMoveSet.of_list
      in
      { game with
        selectedChecker = None
      ; board = newBoard
      ; allowedMoves = newMoves
      }
    else
      rejectGame
