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

let isKing = function
  | King _ -> true
  | _ -> false

let inBounds x y = x >= 0 && x < 8 && y >= 0 && y < 8

let manhattanDistance m = abs (m.toY - m.fromY)

let direction m =
  let run = m.toX - m.fromX in
  let rise = m.toY - m.fromY in
  (run, rise)

(* Requires either a slope of 1 or -1 *)
let validDiagonal m =
  if m.fromX == m.toX || m.fromY == m.toY then
    false
  else
    let (run, rise) = direction m in
    abs rise == abs run

let checkerColor = function
  | King c -> c
  | Pawn c -> c

let otherColor = function
  | Red -> Black
  | _ -> Red

let addChecker x y c b =
  let mask = maskFor x y in
  let color = checkerColor c in
  let king = if isKing c then mask else zero in
  let red = if color = Red then mask else zero in
  let black = if color = Black then mask else zero in
  { b with
    king = BigInteger.or_ b.king @@ `BigInt king
  ; red = BigInteger.or_ b.red @@ `BigInt red
  ; black = BigInteger.or_ b.black @@ `BigInt black
  }

let jumps c m b =
  let oc = otherColor c in
  let (dx, dy) = direction m in
  let steps = manhattanDistance m in
  let rec next_jump_ a s =
    let atX = m.fromX + (dx / steps) * s in
    let atY = m.fromY + (dy / steps) * s in
    let theChecker = checkerAt atX atY b in
    let jumpState =
      ( s = steps
      , s mod 2 = 0
      , theChecker
      , Option.map checkerColor theChecker = Some oc
      )
    in
    match jumpState with
    | (true, true , None  , _   ) -> Some a
    | (_   , true , Some _, _   ) -> None
    | (_   , false, _     , true) -> next_jump_ ((atX,atY) :: a) (s + 1)
    | (_   , true , None  , _   ) -> next_jump_ a (s + 1)
    | _                           -> None
  in
  next_jump_ [] 1 |> Option.map PointSet.of_list

let forward c dy = if c = Black then dy > 0 else dy < 0

let kingRow = function
  | Black -> 7
  | Red -> 0

let nextMove b = { b with next = otherColor b.next }

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
  let atX = s * dx + x in
  let atY = s * dy + y in
  if not @@ inBounds atX atY then
    a
  else
    let tryMove = { fromX = x ; fromY = y ; toX = atX ; toY = atY } in
    let nextA =
      match jumps c tryMove b with
      | Some _ -> ((atX,atY) :: a)
      | _ -> a
    in
    availableJumps nextA (s + 2) c dx dy x y b

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

