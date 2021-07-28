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

module PointOrd = struct
  type t = (int * int)
  let compare a b = Pervasives.compare a b
end

module PointSet = Set.Make(PointOrd)

module CheckersMoveOrd = struct
  type t = checkersMove
  let compare a b = Pervasives.compare a b
end

module CheckersMoveSet = Set.Make(CheckersMoveOrd)

let zero = BigInteger.bigInt (`Int 0)
let one = BigInteger.bigInt (`Int 1)

let emptyBoard = { red = zero ; black = zero ; king = zero ; next = Black }
