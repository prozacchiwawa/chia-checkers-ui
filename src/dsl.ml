open Stringmap
open Either

open Checktypes
open Sexp
open Clvm
open Compiler

type value
  = CInt of BigInteger.t
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
  | APair of (argLabel * argLabel)
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
  | CInt i -> BigInteger.toString i ()
  | CPair (x,y) -> "(" ^ valueToString x ^ " . " ^ valueToString y ^ ")"

let rec convertArg = function
  | Mask i -> CInt i
  | MaxSteps i -> CInt (BigInteger.bigInt @@ `Int i)
  | Step s -> CInt (BigInteger.bigInt @@ `Int s)
  | Point (x,y) -> CPair (CInt (BigInteger.bigInt @@ `Int x), CInt (BigInteger.bigInt @@ `Int y))
  | Color Red -> CInt zero
  | Color Black -> CInt one
  | Checker (Pawn color) ->
    CPair (CInt zero, convertArg (Color color))
  | Checker (King color) ->
    CPair (CInt one, convertArg (Color color))
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
                  , CInt zero
                  )
              )
          )
      )

  | APair (a,b) -> CPair (convertArg a, convertArg b)

  | Maybe (AJust v) -> CPair (convertArg v, CInt zero)
  | Maybe (ANothing) -> CInt zero

  | AList [] -> CInt zero
  | AList (hd :: tl) -> CPair (convertArg hd, convertArg (AList tl))

let rec convertResSome p r =
  match (p,r) with
  | (Maybe (AJust v), CPair (x, CInt zero)) ->
    begin
      match convertResSome v x with
      | Some u -> Some (Maybe (AJust u))
      | _ -> None
    end
  | (Maybe (AJust v), CInt zero) ->
    Some (Maybe ANothing)

  | (AList [pat], CPair (x, next)) ->
    begin
      match (convertResSome pat x, convertResSome p next) with
      | (Some head, Some (AList rest)) ->
        Some (AList (head :: rest))
      | _ -> None
    end
  | (AList [x], CInt zero) ->
    Some (AList [])

  | (APair (a,b), CPair (av,bv)) ->
    begin
      match (convertResSome a av, convertResSome b bv) with
      | (Some ar, Some br) -> Some (APair (ar, br))
      | _ -> None
    end

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
    if BigInteger.equals bi (`BigInt zero) then
      Some (Color Red)
    else
      Some (Color Black)

  | (Checker _, CPair (CInt kind, color)) ->
    begin
      match convertResSome (Color Red) color with
      | Some (Color color) ->
        if BigInteger.equals kind (`BigInt zero) then
          Some (Checker (Pawn color))
        else
          Some (Checker (King color))
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
                , CPair (CInt black, CInt zero)
                )
            )
        )
    ) ->
    Some
      (Board
         { next = if BigInteger.equals color (`BigInt zero) then Red else Black
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
  | Sexp.Nil _ -> CInt zero
  | Sexp.Cons (_,a,b) -> CPair (sexpToValue a, sexpToValue b)
  | a ->
    match sexp_to_bigint a with
    | Some i -> CInt i
    | None -> CInt zero

let verbose = StringSet.of_list ["availableMoves"]

let exec prog fn args =
  let func = StringMap.find fn prog in
  let cvtargs = convertArg (AList args) in
  let serialized = valueToString cvtargs in
  let _ =
    if StringSet.mem func.name verbose then
      Js.log @@ "run " ^ func.name ^ " with " ^ serialized
    else
      ()
  in
  match Clvm.parse_and_run fn func.code serialized with
  | RunOk res ->
    let _ =
      if StringSet.mem func.name verbose then
        Js.log @@ "result " ^ func.name ^ " was " ^ Sexp.to_string res
      else
        ()
    in
    convertRes func.result @@ sexpToValue res
  | RunExn (l,e) -> raise (Failure (Srcloc.toString l ^ ": " ^ Sexp.to_string e))
  | RunError (l,e) -> raise (Failure (Srcloc.toString l ^ ": " ^ e))

