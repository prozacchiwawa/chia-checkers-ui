type ('e, 't) either = Left of 'e | Right of 't

let map f = function
  | Left e -> Left e
  | Right v -> Right (f v)

let bind f = function
  | Left e -> Left e
  | Right v -> f v

let default d = function
  | Left e -> Right d
  | Right v -> Right v
