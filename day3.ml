open Readfile
open Base

let firstOcurrence sack s1' =
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

let findFirstPresentChar sack acc' =
  let sack_mid = String.length sack / 2 in
  let s1 = String.sub sack ~pos:0 ~len:sack_mid in
  let s2 = String.sub sack ~pos:sack_mid ~len:sack_mid in
  let chars_s2 = String.to_list s2 in

  match firstOcurrence chars_s2 s1 with
  | None -> failwith "No common items in sack"
  | Some c -> c :: acc'
;;

let sumPriority cl =
  let res = 0 in
  let rec aux cl res' =
    match cl with
    | [] -> res'
    | hd :: tl ->
      if Char.is_lowercase hd then
        aux tl (res' + Char.to_int hd - 96)
      else
        aux tl (res' + Char.to_int hd - 38)
  in
  aux cl res
;;

(* part 1  *)
let () =
  let inp = read_lines "day3.txt" in
  let char_list =
    List.fold inp ~init:[] ~f:(fun acc sack -> findFirstPresentChar sack acc)
  in
  sumPriority char_list |> Stdio.printf "res : %d\n"
;;
