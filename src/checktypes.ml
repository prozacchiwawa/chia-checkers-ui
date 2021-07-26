type color = Red | Black

type checker
  = King of color
  | Pawn of color

type checkersBoard =
  { red : BigInteger.t
  ; black : BigInteger.t
  ; king : BigInteger.t
  ; next : color
  }

type checkersMove =
  { fromX : int
  ; fromY : int
  ; toX : int
  ; toY : int
  }

module CheckersMoveOrd = struct
  type t = checkersMove
  let compare a b = Pervasives.compare a b
end

module CheckersMoveSet = Set.Make(CheckersMoveOrd)

let zero = BigInteger.bigInt (`Int 0)
let one = BigInteger.bigInt (`Int 1)

let emptyBoard = { red = zero ; black = zero ; king = zero ; next = Black }

let maskFor x y =
  BigInteger.shiftLeft one (y * 8 + x)

let checkerAt x y b =
  let mask = maskFor x y in
  let kingres =
    BigInteger.compare (BigInteger.and_ b.king (`BigInt mask)) (`BigInt zero) != EqualTo
  in
  let redres =
    BigInteger.compare (BigInteger.and_ b.red (`BigInt mask)) (`BigInt zero) != EqualTo
  in
  let blackres =
    BigInteger.compare (BigInteger.and_ b.black (`BigInt mask)) (`BigInt zero) != EqualTo
  in
  match (kingres,redres,blackres) with
  | (true, true, _) -> Some (King Red)
  | (true, _, true) -> Some (King Black)
  | (_   , true, _) -> Some (Pawn Red)
  | (_   , _, true) -> Some (Pawn Black)
  | _ -> None

let removeChecker x y b =
  let mask = maskFor x y in
  let withKing =
    BigInteger.compare
      (BigInteger.and_ b.king (`BigInt mask)) (`BigInt zero) != EqualTo
  in
  let withRed =
    BigInteger.compare
      (BigInteger.and_ b.red (`BigInt mask)) (`BigInt zero) != EqualTo
  in
  let withBlack =
    BigInteger.compare
      (BigInteger.and_ b.black (`BigInt mask)) (`BigInt zero) != EqualTo
  in
  { b with
    king = BigInteger.xor b.king @@ `BigInt (if withKing then mask else zero)
  ; red = BigInteger.xor b.red @@ `BigInt (if withRed then mask else zero)
  ; black = BigInteger.xor b.black @@ `BigInt (if withBlack then mask else zero)
  }

let isKing = function
  | King _ -> true
  | _ -> false

let inBounds x y = x >= 0 && x < 8 && y >= 0 && y < 8

let manhattanDistance m = abs (m.toY - m.fromY)

let direction m =
  let rise = m.toY - m.fromY in
  let run = m.toX - m.fromX in
  (run, rise)

(* Requires either a slope of 1 or -1 *)
let validDiagonal m =
  if m.fromX == m.toX || m.fromY == m.toY then
    false
  else
    let (rise, run) = direction m in
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

module PointOrd = struct
  type t = (int * int)
  let compare a b = Pervasives.compare a b
end

module PointSet = Set.Make(PointOrd)

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
    | (true, true, None, _   ) -> Some a
    | (true, _   , _   , _   ) -> None
    | (_   , true, None, _   ) -> next_jump_ a (s + 1)
    | (_   , true, _   , _   ) -> None
    | (_   , _,  Some _, true) -> next_jump_ ((atX,atY) :: a) (s + 1)
    | _                        -> None
  in
  next_jump_ [] 1 |> Option.map PointSet.of_list

let forward c dy = if c = Black then dy > 0 else dy < 0

let nextMove b = { b with next = otherColor b.next }

let move m b =
  let (dx, dy) = direction m in
  let _ = Js.log "move direction" in
  let _ = Js.log (dx,dy) in
  let md = manhattanDistance m in
  checkerAt m.fromX m.fromY b
  |> Option.map (fun ch -> Js.log 1 ; ch)
  |> Option.filter (fun ch -> b.next = checkerColor ch)
  |> Option.map (fun ch -> Js.log 2 ; ch)
  |> Option.filter (fun _ -> validDiagonal m)
  |> Option.map (fun ch -> Js.log 3 ; ch)
  |> Option.filter (fun _ -> checkerAt m.toX m.toY b = None)
  |> Option.map (fun ch -> Js.log 4 ; ch)
  |> Option.map isKing
  |> Option.map (fun ch -> Js.log 5 ; ch)
  |> Option.filter (fun king -> king || forward b.next dy)
  |> Option.map (fun ch -> Js.log 6 ; ch)
  |> Option.map (fun king -> if king then King b.next else Pawn b.next)
  |> Option.map (fun ch -> Js.log 7 ; ch)
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
  |> Option.map (fun ch -> Js.log 8 ; ch)
  |> Option.map
    (fun (checker,jumps) ->
       jumps
       |> List.fold_left
         (fun b (x,y) -> removeChecker x y b)
         (removeChecker m.fromX m.fromY b)
       |> addChecker m.toX m.toY checker
    )
  |> Option.map (fun ch -> Js.log 9 ; ch)
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
    let nextA =
      match jumps c { fromX = x ; fromY = y ; toX = atX ; toY = atY } b with
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
    |> List.map (fun (dx,dy) -> availableJumps [] 1 (checkerColor c) dx dy x y b)
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

