open Readfile
open Base

let firstMatch sack s1' =
  let rec aux = function
    | [] -> None
    | hd :: rest ->
      if String.contains s1' hd then
        Some hd
      else
        aux rest
  in
  aux sack
;;

let sumPriority c' =
  if Char.is_lowercase c' then
    Char.to_int c' - 96
  else
    Char.to_int c' - 38
;;

let findCommonItem sack acc' =
  let sack_mid = String.length sack / 2 in
  let s1 = String.sub sack ~pos:0 ~len:sack_mid in
  let s2 = String.sub sack ~pos:sack_mid ~len:sack_mid in
  let chars_s2 = String.to_list s2 in

  match firstMatch chars_s2 s1 with
  | None -> failwith "No common items in sack"
  | Some c -> sumPriority c + acc'
;;

let inp = read_lines "day3.txt"

(* part 1 *)
let () =
  List.fold inp ~init:0 ~f:(fun acc sack -> findCommonItem sack acc)
  |> Stdio.printf "res : %d\n"
;;

type group = {
  s1: string;
  s2: string;
  s3: string;
}

let matches badge_cand sack =
  let rec aux matched_chars cand =
    match cand with
    | [] -> matched_chars
    | hd :: rest ->
      if String.contains sack hd then
        aux (hd :: matched_chars) rest
      else
        aux matched_chars rest
  in
  aux [] badge_cand
;;

let rec makeGroups = function
  | [] -> []
  | a :: b :: c :: tl -> { s1 = a; s2 = b; s3 = c } :: makeGroups tl
  | _ -> failwith "Cant make groups of 3"
;;

(* let alternative = List.groupi inp ~break:(fun i _ _ -> i % 3 = 0) *)

let findAllBadge group' res' =
  let possible_badges = String.to_list group'.s1 in
  match matches possible_badges group'.s2 with
  | [] -> failwith "No common badges with s2"
  | badge_candidates ->
    (match firstMatch badge_candidates group'.s3 with
     | Some badge -> sumPriority badge + res'
     | None -> failwith "No common badges with s3")
;;

(* part2 *)
let () =
  List.fold (makeGroups inp) ~init:0 ~f:(fun acc group' -> findAllBadge group' acc)
  |> Stdio.printf "res : %d\n"
;;
