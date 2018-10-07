open Core

let rec qsort = function
  | []      -> []
  | x :: xs ->
    List.partition_tf ~f:(fun y -> y < x) xs
    |> (fun (s, l) -> (qsort s) @ (x :: qsort l))
