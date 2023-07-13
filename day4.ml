open Base

let makePair str' =
  let map_touple f ((a, b), (c, d)) = (f a, f b), (f c, f d) in

  let splitAndTouple str' char' =
    match String.split str' ~on:char' with
    | [t1; t2] -> t1, t2
    | []
    | _ ->
      failwith "Wrong format"
  in
  let a, b = splitAndTouple str' ',' in
  map_touple Int.of_string (splitAndTouple a '-', splitAndTouple b '-')
;;

let checkIfContained ((z1x, z1y), (z2x, z2y)) =
  if (z1x >= z2x && z1y <= z2y) || (z2x >= z1x && z2y <= z1y) then
    1
  else
    0
;;

let checkOverlap ((z1x, z1y), (z2x, z2y)) =
  let pair = (z1x, z1y), (z2x, z2y) in
  if checkIfContained pair = 1 || (z1y >= z2x && z2y >= z1x) then
    1
  else
    0
;;

let inp = Readfile.read_lines "day4.txt"

(* part 1 *)
let () =
  List.fold inp ~init:0 ~f:(fun acc pair' -> (makePair pair' |> checkIfContained) + acc)
  |> Stdio.printf "Ans1 : %d\n"
;;

(* part 2 *)
let () =
  List.fold inp ~init:0 ~f:(fun acc pair' -> (makePair pair' |> checkOverlap) + acc)
  |> Stdio.printf "Ans2 : %d\n"
;;
