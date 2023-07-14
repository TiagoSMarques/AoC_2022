open Base

let inp = Readfile.read_lines "5.txt"

(* open Str *)

let storage, moves =
  (* List.group ~break:(fun _ s -> String.length s % 2 = 0) inp;; *)
  let groups = List.group inp ~break:(fun _ s -> String.compare s "" = 0) in
  match groups with
  | storage' :: moves' :: _ -> storage', moves'
  | _ -> failwith "Wrong format - no separation"
;;

(* Readfile.print_listof_strs inp *)

(* | _ -> ([], []) *)
Readfile.print_listof_strs moves
(* let s = List.findi inp ~f:(fun i a' -> (String.compare a' "\n" = 0));
let a, b = List.split_n 
 *)
