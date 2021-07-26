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
    { game with
      selectedChecker = Some (x,y)
    }
  | Some (bx,by) ->
    let moveTo =
      match checkerAt bx by game.board with
      | Some ch ->
        availableMovesForChecker ch bx by game.board
        |> PointSet.of_list
      | None -> PointSet.empty
    in
    if PointSet.mem (x,y) moveTo then
      let m = { fromX = bx ; fromY = by ; toX = x ; toY = y } in
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
