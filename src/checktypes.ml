type color = Red | Black

type checker
  = King of color
  | Pawn of color

type checkersBoard =
  { red : BigInteger.t
  ; black : BigInteger.t
  ; king : BigInteger.t
  }

type checkersMove =
  { fromX : int
  ; fromY : int
  ; toX : int
  ; toY : int
  }

let zero = BigInteger.bigInt (`Int 0)
let one = BigInteger.bigInt (`Int 1)

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
  { king = BigInteger.xor b.king @@ `BigInt (if withKing then mask else zero)
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
  (rise, run)

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
  { king = BigInteger.or_ b.king @@ `BigInt king
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
