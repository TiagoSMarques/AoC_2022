(* open Readfile

let inp = read_lines "day3.txt"

let sumPriorities l total =
  match l with
  | [] -> []
  | hd :: tl ->
    let sack_mid = String.length hd / 2 in
    let s1 = String.sub hd 0 sack_mid in
    let s2 = String.sub hd sack_mid sack_mid in
    let chars_s2 = String.to_seq s2 in
    let list_matches = Seq.map (fun c -> String.index s1 c) chars_s2 in 
;; *)

String.get_int8 "Z" 0 |> print_int

let hd = "helloo"
let sack_mid = String.length hd / 2
let s1 = String.sub hd 0 sack_mid
let s2 = String.sub hd sack_mid sack_mid
let chars_s2 = String.to_seq s2

let list_matches =
  Seq.exists
    (fun c ->
      match String.index_opt s1 c with
      | Some _ -> true
      | None -> false)
    chars_s2
;;

let () = Stdio.printf "s1: %s \n" s1
let () = Stdio.printf "s2: %s \n" s2
let () = Stdio.printf "\n\n %d" (Seq.length list_matches)
(* let () = Seq.iter print_char chars_s2 *)
(* let () = Seq.iter (Stdio.printf "%i \n") list_matches *)

(* let () = sumPoints_1 inp 0 |> printf "Day2 p1 : %d \n" *)
