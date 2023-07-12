open Readfile

let findFirstPresentChar sack l_chars =
  let open Base in
  let sack_mid = String.length sack / 2 in
  let s1 = String.sub sack ~pos:0 ~len:sack_mid in
  let s2 = String.sub sack ~pos:sack_mid ~len:sack_mid in
  let chars = String.to_list s2 in

  let rec firstOcurrence = function
    | [] -> failwith "No same items in the 2 compartments"
    | hd :: rest ->
      if String.contains s1 hd then
        hd
      else
        firstOcurrence rest
  in
  firstOcurrence chars :: l_chars
;;

String.get_int8 "Z" 0 |> print_int

let calPriority = List.map ()

(* let hd = "helloo" *)

let inp = read_lines "day3.txt"
let char_list = List.fold_left (fun acc sack -> findFirstPresentChar sack acc) [] inp
let () = List.iter print_char char_list
(* 
let () = Stdio.printf "s1: %s \n" s1
let () = Stdio.printf "s2: %s \n" s2
let () = Stdio.printf "\n\n %d" (Seq.length list_matches) *)
(* let () = Seq.iter print_char chars_s2 *)
(* let () = Seq.iter (Stdio.printf "%i \n") list_matches *)

(* let () = sumPoints_1 inp 0 |> printf "Day2 p1 : %d \n" *)
