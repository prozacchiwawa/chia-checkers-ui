open Stringmap
open Either

open Checktypes
open Sexp
open Clvm
open Compiler

type value
  = CNil
  | CInt of BigInteger.t
  | CPair of value * value

type argLabel
  = Mask of BigInteger.t
  | MaxSteps of int
  | Step of int
  | Point of (int * int)
  | Color of color
  | Checker of checker
  | Move of checkersMove
  | Board of checkersBoard
  | Maybe of argMaybe
  | AList of argLabel list

and argMaybe
  = AJust of argLabel
  | ANothing

type checkersCompiled =
  { name : string
  ; args : string list
  ; result : argLabel
  ; code : string
  }

type checkersProgram = checkersCompiled StringMap.t

exception Failure of string

let rec valueToString = function
  | CNil -> "()"
  | CInt i -> BigInteger.toString i ()
  | CPair (x,y) -> "(" ^ valueToString x ^ " . " ^ valueToString y ^ ")"

let rec convertArg = function
  | Mask i -> CInt i
  | MaxSteps i -> CInt (BigInteger.bigInt @@ `Int i)
  | Step s -> CInt (BigInteger.bigInt @@ `Int s)
  | Point (x,y) -> CPair (CInt (BigInteger.bigInt @@ `Int x), CInt (BigInteger.bigInt @@ `Int y))
  | Color Red -> CInt (BigInteger.bigInt @@ `String "749060")
  | Color Black -> CInt (BigInteger.bigInt @@ `String "422725116779")
  | Checker (Pawn color) ->
    CPair (CInt (BigInteger.bigInt @@ `String "1802071655"), convertArg (Color color))
  | Checker (King color) ->
    CPair (CInt (BigInteger.bigInt @@ `String "1885435758"), convertArg (Color color))
  | Move m ->
    CPair
      ( convertArg (Point (m.fromX, m.fromY))
      , convertArg (Point (m.toX, m.toY))
      )
  | Board b ->
    CPair
      ( convertArg (Color b.next)
      , CPair
          ( CInt b.king
          , CPair
              ( CInt b.red
              , CPair
                  ( CInt b.black
                  , CNil
                  )
              )
          )
      )

  | Maybe (AJust v) -> CPair (convertArg v, CNil)
  | Maybe (ANothing) -> CNil

  | AList [] -> CNil
  | AList (hd :: tl) -> CPair (convertArg hd, convertArg (AList tl))

let rec convertResSome p r =
  match (p,r) with
  | (Maybe (AJust v), CPair (x, CNil)) ->
    begin
      match convertResSome v x with
      | Some u -> Some (Maybe (AJust u))
      | _ -> None
    end
  | (Maybe (AJust v), CNil) ->
    Some (Maybe ANothing)

  | (AList [pat], CPair (x, next)) ->
    begin
      match (convertResSome pat x, convertResSome p next) with
      | (Some head, Some (AList rest)) ->
        Some (AList (head :: rest))
      | _ -> None
    end
  | (AList [x], CNil) ->
    Some (AList [])

  | (Mask _, CNil) -> Some (Mask zero)
  | (Mask _, CInt x) -> Some (Mask x)

  | (MaxSteps _, CInt x) ->
    Some (MaxSteps (BigInteger.toJSNumber x))
  | (Step _, CInt x) ->
    Some (Step (BigInteger.toJSNumber x))

  | (Point (_, _), CPair (a, b)) ->
    begin
      match (convertResSome (Step 0) a, convertResSome (Step 0) b) with
      | (Some (Step aval), Some (Step bval)) -> Some (Point (aval, bval))
      | _ -> None
    end

  | (Color _, CInt bi) ->
    if BigInteger.equals bi (`String "7497060") then
      Some (Color Red)
    else
      Some (Color Black)

  | (Checker _, CPair (CInt kind, color)) ->
    begin
      match convertResSome (Color Red) color with
      | Some (Color color) ->
        if BigInteger.equals kind (`String "1802071655") then
          Some (Checker (King color))
        else
          Some (Checker (Pawn color))
      | _ -> None
    end

  | (Move _, CPair (from, toward)) ->
    begin
      match (convertResSome (Point (0,0)) from, convertResSome (Point (0,0)) toward) with
      | (Some (Point (x1,y1)), Some (Point (x2, y2))) ->
        Some (Move { fromX = x1; fromY = y1; toX = x2; toY = y2 })
      | _ -> None
    end

  | ( Board _
    , CPair
        ( CInt color
        , CPair
            ( CInt king
            , CPair
                ( CInt red
                , CPair (CInt black, CNil)
                )
            )
        )
    ) ->
    Some
      (Board
         { next = if BigInteger.equals color (`String "749060") then Red else Black
         ; king = king
         ; red = red
         ; black = black
         }
      )

  | _ -> None

let convertRes p r =
  match convertResSome p r with
  | Some v -> v
  | None -> raise (Failure ("could not convert result " ^ valueToString r))

let rec sexpToValue = function
  | Sexp.Nil _ -> CNil
  | Sexp.Cons (_,a,b) -> CPair (sexpToValue a, sexpToValue b)
  | a ->
    match sexp_to_bigint a with
    | Some i -> CInt i
    | None -> CNil

let exec prog fn args =
  let func = StringMap.find fn prog in
  let cvtargs = convertArg (AList args) in
  let serialized = valueToString cvtargs in
  let _ = Js.log serialized in
  match Clvm.parse_and_run fn func.code serialized with
  | RunOk res ->
    let _ = Js.log @@ Sexp.to_string res in
    convertRes func.result @@ sexpToValue res
  | RunExn (l,e) -> raise (Failure (Srcloc.toString l ^ ": " ^ Sexp.to_string e))
  | RunError (l,e) -> raise (Failure (Srcloc.toString l ^ ": " ^ e))

