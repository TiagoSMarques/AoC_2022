(* open Readfile


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

let findFirstPresentChar sack=

let sack_mid = String.length sack / 2 in 
let str = String.sub sack 0 sack_mid in 
let s2 = String.sub sack sack_mid sack_mid in 
let chars = List.of_seq (String.to_seq s2) in

let rec aux = function
    | [] -> '!'
    | hd :: rest ->
        if String.contains str hd then
        hd
        else
          aux rest
  in
  aux chars;;

String.get_int8 "Z" 0 |> print_int

let hd = "helloo"

let inp = read_lines "day3.txt";
let char_list = List.map (fun (sack) -> findFirstPresentChar sack) inp;;



let () = Stdio.printf "s1: %s \n" s1
let () = Stdio.printf "s2: %s \n" s2
let () = Stdio.printf "\n\n %d" (Seq.length list_matches)
(* let () = Seq.iter print_char chars_s2 *)
(* let () = Seq.iter (Stdio.printf "%i \n") list_matches *)

(* let () = sumPoints_1 inp 0 |> printf "Day2 p1 : %d \n" *)
