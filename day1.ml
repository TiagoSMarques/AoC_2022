open Base
open Stdio
open Readfile

let inputFormat input =
  List.map input ~f:(fun x ->
    match x with
    | "" -> 0
    | _ -> Int.of_string x)
;;

let sumCals l =
  List.fold_right l ~init:[ 0 ] ~f:(fun item acc ->
    match item with
    | 0 -> item :: acc
    | _ ->
      (* add item to the the first element is results  *)
      (match acc with
       | [] -> []
       | hd :: tl -> (hd + item) :: tl))
;;

let findMax l_in = List.fold l_in ~init:0 ~f:(fun acc x -> Int.max x acc)

(* Part 1 *)
let () =
  read_lines "day1.txt" |> inputFormat |> sumCals |> findMax |> printf "Day 1 p1 %i\n"
;;

(* Part 2 *)
let rec sum = function
  | [] -> 0
  | x :: xs -> x + sum xs
;;

let () =
  let sortedCals =
    read_lines "day1.txt"
    |> inputFormat
    |> sumCals
    |> List.sort ~compare:(fun x y -> compare y x)
    (* sort in descending order *)
  in
  List.take sortedCals 3 |> sum |> printf "Day 1 p2 %i\n"
;;
