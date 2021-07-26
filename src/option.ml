let map f = function
  | Some x -> Some (f x)
  | None -> None

let bind f = function
  | Some x -> f x
  | None -> None

let filter f = function
  | Some x -> if f x then Some x else None
  | None -> None

let default v = function
  | Some x -> x
  | _ -> v
