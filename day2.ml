open Readfile
open Stdio
(* open Base.Poly *)
(* for Rock, Y for Paper, and Z for Scissors. *)

(* Part 1 *)

let rec sumPoints_1 plays points =
  match plays with
  | [] -> points
  | hd :: tl ->
    (match Str.(split (regexp " ")) hd with
     | []
     | _ :: [] ->
       failwith "Wrong size"
     | op :: pl :: _ ->
       (match pl with
        | "X" ->
          (match op with
           | "C" -> sumPoints_1 tl (points + 1 + 6)
           | "A" -> sumPoints_1 tl (points + 1 + 3)
           | _ -> sumPoints_1 tl (points + 1 + 0))
        | "Y" ->
          (match op with
           | "A" -> sumPoints_1 tl (points + 2 + 6)
           | "B" -> sumPoints_1 tl (points + 2 + 3)
           | _ -> sumPoints_1 tl (points + 2 + 0))
        | "Z" ->
          (match op with
           | "B" -> sumPoints_1 tl (points + 3 + 6)
           | "C" -> sumPoints_1 tl (points + 3 + 3)
           | _ -> sumPoints_1 tl (points + 3 + 0))
        | _ -> failwith "incorrect code"))
;;

let inp = read_lines "day2.txt"
let () = sumPoints_1 inp 0 |> printf "Day2 p1 : %d \n"

(* Part 2 *)
let rec sumPoints_2 plays points =
  match plays with
  | [] -> points
  | hd :: tl ->
    (match Str.(split (regexp " ")) hd with
     | []
     | _ :: [] ->
       failwith "Wrong size"
     | op :: pl :: _ ->
       (match pl with
        | "X" ->
          (match op with
           | "C" -> sumPoints_2 tl (points + 2 + 0)
           | "A" -> sumPoints_2 tl (points + 3 + 0)
           | _ -> sumPoints_2 tl (points + 1 + 0))
        | "Y" ->
          (match op with
           | "A" -> sumPoints_2 tl (points + 1 + 3)
           | "B" -> sumPoints_2 tl (points + 2 + 3)
           | _ -> sumPoints_2 tl (points + 3 + 3))
        | "Z" ->
          (match op with
           | "B" -> sumPoints_2 tl (points + 3 + 6)
           | "C" -> sumPoints_2 tl (points + 1 + 6)
           | _ -> sumPoints_2 tl (points + 2 + 6))
        | _ -> failwith "incorrect code"))
;;

let () = sumPoints_2 inp 0 |> printf "Day2 p2 : %d \n"
