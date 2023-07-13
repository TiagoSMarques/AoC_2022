open Base

let inp = Readfile.read_lines "day4.txt"

(* let getAssignements file_row=  *)
let i = "2-4,6-8"

let splitAndTouple str' char' =
  match String.split str' ~on:char' with
  | [t1; t2] -> t1, t2
  | []
  | _ ->
    failwith "No wrong format"
;;

let (z1x, z2x), (z1y, z2y) =
  let a, b = splitAndTouple i ',' in
  splitAndTouple a '-', splitAndTouple b '-'
;;
