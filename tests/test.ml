open Checktypes

let red44 = { red = maskFor 4 4 ; black = zero ; king = zero ; next = Black }
let blackKing44 = { red = zero ; black = maskFor 4 4 ; king = maskFor 4 4 ; next = Black }
let jumpable2 =
  emptyBoard
  |> addChecker 3 3 (Pawn Red)
  |> addChecker 5 5 (Pawn Red)

let tests =
  [ ( "empty board doesn't contain pieces"
    , (fun _ -> checkerAt 4 4 emptyBoard = None)
    )
  ; ( "we detect a red piece"
    , (fun _ -> checkerAt 4 4 red44 = Some (Pawn Red))
    )
  ; ( "we detect a king"
    , (fun _ -> checkerAt 4 4 blackKing44 = Some (King Black))
    )
  ; ( "we can detect out of bounds -1 3"
    , (fun _ -> inBounds (-1) 3 == false)
    )
  ; ( "we can detect in bounds 2 3"
    , (fun _ -> inBounds 2 3 == true)
    )
  ; ( "manhattan distance"
    , (fun _ ->
         manhattanDistance { fromX = 2 ; fromY = 3 ; toX = 4 ; toY = 5 } == 2
      )
    )
  ; ( "direction"
    , (fun _ ->
         let d = direction { fromX = 2 ; fromY = 3 ; toX = 4 ; toY = 5 } in
         d = (2,2)
      )
    )
  ; ( "is valid diagonal"
    , (fun _ ->
         validDiagonal { fromX = 2 ; fromY = 3 ; toX = 4 ; toY = 5 } == true
      )
    )
  ; ( "not valid diagonal"
    , (fun _ ->
         validDiagonal { fromX = 2 ; fromY = 3 ; toX = 3 ; toY = 5 } == false
      )
    )
  ; ( "can also remove"
    , (fun _ ->
         checkerAt 4 4 (removeChecker 4 4 blackKing44) = None
      )
    )
  ; ( "can do 2 jumps"
    , (fun _ ->
         let theJumps =
           jumps
             Black
             { fromX = 2 ; fromY = 2 ; toX = 6 ; toY = 6 }
             jumpable2
         in
         let wantJumps = PointSet.of_list [(3,3);(5,5)] in
         match theJumps with
         | Some tj -> PointSet.equal tj wantJumps
         | _ -> false
      )
    )
  ]

let all l =
  let f = ref true in
  let _ = l |> List.iter (fun v -> f := v && !f) in
  !f

let main _ =
  let results =
    tests
    |> List.mapi
      (fun i (name, tf) ->
         if tf () then
           true
         else
           let _ = Printf.printf "test failed: %s\n" name in
           false
      )
  in
  if all results then
    Printf.printf "\n\nPASS\n\n"
  else
    Printf.printf "\n\nFAIL\n\n"

let _ = main ()
