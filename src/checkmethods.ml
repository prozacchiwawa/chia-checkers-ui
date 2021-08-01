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

let move m b =
  let (dx, dy) = direction m in
  let md = manhattanDistance m in
  checkerAt m.fromX m.fromY b
  |> Xoption.filter (fun ch -> b.next = checkerColor ch)
  |> Xoption.filter (fun _ -> validDiagonal m)
  |> Xoption.filter (fun _ -> checkerAt m.toX m.toY b = None)
  |> Option.map isKing
  |> Xoption.filter (fun king -> king || forward b.next dy)
  |> Option.map (fun king -> if king then King b.next else Pawn b.next)
  |> Option.bind
    (fun checker ->
       let jumps =
         if md == 1 then
           Some []
         else
           jumps b.next m b |> Option.map PointSet.elements
       in
       jumps |> Option.map (fun j -> (checker,j))
    )
  |> Option.map
    (fun (checker,jumps) ->
       let color = checkerColor checker in
       let promoted =
         if m.toY = kingRow color then
           King color
         else
           checker
       in
       jumps
       |> List.fold_left
         (fun b (x,y) -> removeChecker x y b)
         (removeChecker m.fromX m.fromY b)
       |> addChecker m.toX m.toY promoted
    )
  |> Option.map nextMove

(* For some direction dx, dy, check each possible jump to see if jumps
 * detects that it's a valid jump
 *)
let rec availableJumps a s c dx dy x y b =
  match exec program "availableJumps" [AList (List.map (fun p -> Point p) a); Step s; Color c; Step dx; Step dy; Step x; Step y; Board b] with
  | AList l ->
    List.map
      (function
        | Point p -> p
      )
      l

let availableMovesForChecker c x y b =
  let allOneSpaceMoves = [(-1,1);(-1,-1);(1,1);(1,-1)] in
  let oneSpaceMovesRaw =
    match c with
    | King _ -> allOneSpaceMoves
    | Pawn color ->
      List.filter (fun (_,dy) -> forward color dy) allOneSpaceMoves
  in
  let oneSpaceMovesInBounds =
    List.filter
      (fun (dx,dy) -> inBounds (x + dx) (y + dy))
      oneSpaceMovesRaw
  in
  let oneSpaceMovesNotBlocked =
    List.filter
      (fun (dx,dy) -> checkerAt (x + dx) (y + dy) b = None)
      oneSpaceMovesInBounds
  in
  let jumps =
    oneSpaceMovesInBounds
    |> List.map (fun (dx,dy) -> availableJumps [] 2 (checkerColor c) dx dy x y b)
    |> List.concat
  in
  List.concat
    [ jumps
    ; oneSpaceMovesNotBlocked
      |> List.map (fun (dx,dy) -> (x + dx, y + dy))
    ]

let rec listCheckersWithColor n c b =
  if n >= 64 then
    []
  else
    let x = n mod 8 in
    let y = n / 8 in
    let chq = checkerAt x y b in
    let rest = listCheckersWithColor (n+1) c b in
    let head =
      match chq with
      | Some (King pc) -> if pc = c then [(x,y,King c)] else []
      | Some (Pawn pc) -> if pc = c then [(x,y,Pawn c)] else []
      | _ -> []
    in
    List.concat [head ; rest]

let colorToString = function
  | Red -> "red"
  | Black -> "black"

let availableMoves b =
  let withColor = listCheckersWithColor 0 b.next b in
  withColor
  |> List.map
    (fun (x,y,c) ->
       availableMovesForChecker c x y b
       |> List.map
         (fun (nx,ny) -> { fromX = x ; fromY = y; toX = nx ; toY = ny })
    )
  |> List.concat

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

