module type Ord = sig
  type t
  val compare : t -> t -> int
end

module StringMap = Map.Make(String)

module MapBuilder (X : Ord) = struct
  module MapT = Map.Make(X)

  let build l =
    List.fold_left
      (fun m (n,v) -> MapT.add n v m)
      MapT.empty
      l
end

module StringMapBuilder = MapBuilder(String)
