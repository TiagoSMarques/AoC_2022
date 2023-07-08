open Base
open Stdio
open Readfile

let inputFormat input =
  List.map input ~f:(fun x ->
      if String.compare x "" = 0 then 0 else Int.of_string x)

let findMax l_in = List.fold l_in ~init:0 ~f:(fun acc x -> Int.max x acc)

(* Another way of finding max its the same speed*)
(* let findMax_2 l_in =
   List.max_elt l_in ~compare:Int.compare |> function None -> 0 | Some n -> n *)

let sumCals l =
  let incFirstItem l n = match l with [] -> [] | hd :: tl -> (hd + n) :: tl in
  List.fold_right l ~init:[ 0 ] ~f:(fun item acc ->
      match item with 0 -> item :: acc | _ -> incFirstItem acc item)

(* Part 1 *)
let () =
  read_lines "day1.txt" |> inputFormat |> sumCals |> findMax
  |> printf "Day 1 p1 %i\n"

(* Part 2 *)
let rec sum = function [] -> 0 | x :: xs -> x + sum xs

let () =
  let sortedCals =
    read_lines "day1.txt" |> inputFormat |> sumCals
    |> List.sort ~compare:(fun x y -> compare y x)
    (* sort in descending order *)
  in
  List.take sortedCals 3 |> sum |> printf "Day 1 p2 %i\n"
