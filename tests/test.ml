open Checktypes
open Checkmethods

let tests =
  [ ]

let all l =
  let f = ref true in
  let _ = l |> List.iter (fun v -> f := v && !f) in
  !f

let main _ =
  let results =
    tests
    |> List.mapi
      (fun i (name, tf) ->
         if tf () then
           true
         else
           let _ = Printf.printf "test failed: %s\n" name in
           false
      )
  in
  if all results then
    Printf.printf "\n\nPASS\n\n"
  else
    Printf.printf "\n\nFAIL\n\n"

let _ = main ()
