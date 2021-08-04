open Checktypes
open Clvmcheckers
open Dsl

let maskFor x y =
  match exec program "maskFor" [Point (x,y)] with
  | Mask x -> x

let checkerAt x y b =
  match exec program "checkerAt" [Point (x,y); Board b] with
  | Maybe (AJust (Checker c)) -> Some c
  | Maybe ANothing -> None

let removeChecker x y b =
  match exec program "removeChecker" [Point (x,y); Board b] with
  | Board b -> b

let isKing c =
  match exec program "isKing" [Checker c] with
  | MaxSteps 0 -> false
  | _ -> true

let inBounds x y =
  match exec program "inBounds" [Point (x,y)] with
  | MaxSteps 0 -> false
  | _ -> true

let manhattanDistance m =
  match exec program "manhattanDistance" [Move m] with
  | MaxSteps n -> n

let direction m =
  match exec program "direction" [Move m] with
  | Point p -> p

(* Requires either a slope of 1 or -1 *)
let validDiagonal m =
  match exec program "validDiagonal" [Move m] with
  | MaxSteps 0 -> false
  | _ -> true

let checkerColor ch =
  match exec program "checkerColor" [Checker ch] with
  | Color c -> c

let otherColor c =
  match exec program "otherColor" [Color c] with
  | Color c -> c

let addChecker x y c b =
  match exec program "addChecker" [Point (x,y); Checker c; Board b] with
  | Board b -> b

let jumps c m b =
  match exec program "jumps" [Color c; Move m; Board b] with
  | Maybe (AJust (AList l)) ->
    Some (List.map (fun (Point (x,y)) -> (x,y)) l |> PointSet.of_list)
  | Maybe _ -> None

let forward c dy =
  match exec program "forward" [Color c;Step dy] with
  | MaxSteps 0 -> false
  | _ -> true

let kingRow c =
  match exec program "kingRow" [Color c] with
  | MaxSteps n -> n

let nextMove b =
  match exec program "nextMove" [Board b] with
  | Board b -> b

let optionToMaybe f = function
  | Some x -> Maybe (AJust (f x))
  | _ -> Maybe ANothing

let optionFromMaybe (f : argLabel -> 'a) : argLabel -> 'a option = function
  | Maybe (AJust x) -> Some (f x)
  | _ -> None

let filterCorrectColor b (ch : checker option) : checker option =
  exec program "filterCorrectColor" [optionToMaybe (fun x -> Checker x) ch; Board b]
  |> optionFromMaybe
    (function
      | Checker x -> x
    )

let filterValidDiagonal m ch =
  exec program "filterValidDiagonal" [optionToMaybe (fun x -> Checker x) ch; Move m]
  |> optionFromMaybe
    (function
      | Checker x -> x
    )

let filterSpaceIsFree m b ch =
  exec program "filterSpaceIsFree" [optionToMaybe (fun x -> Checker x) ch; Move m; Board b]
  |> optionFromMaybe
    (function
      | Checker x -> x
    )

let filterToIsKing ch =
  exec program "filterToIsKing" [optionToMaybe (fun x -> Checker x) ch]
  |> optionFromMaybe
    (function
      | Step 0 -> false
      | _ -> true
    )

let filterKingOrForward m b k =
  exec program "filterKingOrForward" [Move m; Board b; optionToMaybe (fun x -> if x then Step 1 else Step 0) k]
  |> optionFromMaybe
    (function
      | Step 0 -> false
      | _ -> true
    )

let mapKingToChecker b k =
  exec program "mapKingToChecker" [Board b; optionToMaybe (fun x -> if x then Step 1 else Step 0) k]
  |> optionFromMaybe
    (function
      | Checker ch -> ch
    )

let rejectIfLongDistanceAndNoJumps m b ch : (checker * ((int * int) list)) option =
  exec program "rejectIfLongDistanceAndNoJumps"
    [ Move m
    ; Board b
    ; optionToMaybe (fun ch -> Checker ch) ch
    ]
  |> optionFromMaybe
    (function
      | APair (Checker ch, AList l) ->
        ( ch
        , List.map
            (function
              | Point (x,y) -> (x,y)
            )
            l
        )
    )

let updateBoardWithMove m b tmj : checkersBoard option =
  exec program "updateBoardWithMove" [Move m; Board b; optionToMaybe (fun (ch,pts) -> APair (Checker ch, AList (List.map (fun pt -> Point pt) pts))) tmj]
  |> optionFromMaybe
    (function
      | Board b -> b
    )

let move m b =
  match exec program "move" [Move m; Board b] with
  | Maybe (AJust (Board b)) -> Some b
  | _ -> None

(* For some direction dx, dy, check each possible jump to see if jumps
 * detects that it's a valid jump
 *)
let availableJumps a s c dx dy x y b =
  match exec program "availableJumps" [AList (List.map (fun p -> Point p) a); Step s; Color c; Step dx; Step dy; Step x; Step y; Board b] with
  | AList l ->
    List.map
      (function
        | Point p -> p
      )
      l

let rec listCheckersWithColor n c b =
  match exec program "listCheckersWithColor" [Step n; Color c; Board b] with
  | AList l ->
    List.map
      (function
        | APair (Point (x,y), Checker c) ->
          (x,y,c)
      )
      l

let oneSpaceMovesRaw checker =
  match exec program "oneSpaceMovesRaw" [Checker checker] with
  | AList l ->
    List.map
      (function
        | Point (x,y) -> (x,y)
      )
      l

let oneSpaceMovesInBounds pt l =
  match exec program "oneSpaceMovesInBounds" [Point pt; AList (List.map (fun (x,y) -> Point (x,y)) l)] with
  | AList l ->
    List.map
      (function
        | Point (x,y) -> (x,y)
      )
      l

let oneSpaceMovesNotBlocked pt b l =
  match exec program "oneSpaceMovesNotBlocked" [Point pt; Board b; AList (List.map (fun (x,y) -> Point (x,y)) l)] with
  | AList l ->
    List.map
      (function
        | Point (x,y) -> (x,y)
      )
      l

let allowedJumps c pt b l =
  match exec program "allowedJumps" [Color c; Point pt; Board b; AList (List.map (fun (x,y) -> Point (x,y)) l)] with
  | AList l ->
    List.map
      (function
        | Point (x,y) -> (x,y)
      )
      l

let availableMovesForChecker ch x y b =
  match exec program "availableMovesForChecker" [Checker ch; Point (x,y); Board b] with
  | AList l ->
    List.map
      (function
        | Point (x,y) -> (x,y)
      )
      l

let availableMoves b =
  match exec program "availableMoves" [Board b] with
  | AList ms ->
    List.map
      (function
        | Move m -> m
      )
      ms

let colorToString = function
  | Red -> "red"
  | Black -> "black"

let standardBoard =
  [ (0,0,Black)
  ; (2,0,Black)
  ; (4,0,Black)
  ; (6,0,Black)
  ; (1,1,Black)
  ; (3,1,Black)
  ; (5,1,Black)
  ; (7,1,Black)
  ; (0,2,Black)
  ; (2,2,Black)
  ; (4,2,Black)
  ; (6,2,Black)

  ; (1,5,Red)
  ; (3,5,Red)
  ; (5,5,Red)
  ; (7,5,Red)
  ; (0,6,Red)
  ; (2,6,Red)
  ; (4,6,Red)
  ; (6,6,Red)
  ; (1,7,Red)
  ; (3,7,Red)
  ; (5,7,Red)
  ; (7,7,Red)
  ]
  |> List.fold_left
    (fun b (x,y,c) -> addChecker x y (Pawn c) b)
    emptyBoard

