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
  match game.selectedChecker with
  | None ->
    let moveTo =
      match checkerAt x y game.board with
      | Some ch ->
        availableMovesForChecker ch x y game.board
        |> List.map (fun (ex,ey) -> { fromX = x ; fromY = y ; toX = ex ; toY = ey })
        |> CheckersMoveSet.of_list
      | None -> CheckersMoveSet.empty
    in
    let _ = Js.log "move to" in
    let _ = Js.log @@ Array.of_list @@ CheckersMoveSet.elements moveTo in
    { game with
      selectedChecker = Some (x,y)
    ; allowedMoves = moveTo
    }
  | Some (bx,by) ->
    let moveTo =
      match checkerAt bx by game.board with
      | Some ch ->
        availableMovesForChecker ch bx by game.board
        |> PointSet.of_list
      | None -> PointSet.empty
    in
    let m = { fromX = bx ; fromY = by ; toX = x ; toY = y } in
    if PointSet.mem (x,y) moveTo then
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
      |> Option.default { game with selectedChecker = None }
    else
      { game with selectedChecker = None }
