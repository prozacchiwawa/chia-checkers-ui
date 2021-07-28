let zip a b =
  let rec zip_ a b =
    match (a,b) with
    | ([], _) -> []
    | (_, []) -> []
    | (h1 :: t1, h2 :: t2) -> (h1,h2) :: zip_ t1 t2
  in
  zip_ a b
