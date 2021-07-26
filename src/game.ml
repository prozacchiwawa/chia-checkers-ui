open Checktypes

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
    let _ = Js.log game.allowedMoves in
    if CheckersMoveSet.mem m game.allowedMoves then
      let allMoves =
        availableMoves game.board
        |> CheckersMoveSet.of_list
      in
      move m game.board
      |> Option.map
        (fun newBoard ->
           let newMoves =
             availableMoves newBoard
             |> CheckersMoveSet.of_list
           in
           { game with
             selectedChecker = None
           ; board = newBoard
           ; allowedMoves = newMoves
           }
        )
      |> Option.default
        { game with
          selectedChecker = None
        ; allowedMoves = allMoves
        }
    else
      { game with selectedChecker = None }
