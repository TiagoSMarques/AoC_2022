open Base

let inp = Readfile.read_lines "day6.txt"
let b = "asdfxii"
let c = String.drop_suffix b

let findFirstDup str =
  let sl = String.length str in
  let rec aux i =
    if String.count str ~f:(fun c2 -> Char.( = ) str.[i] c2) > 1 then
      i
    else if i >= sl - 1 then
      failwith "No duplicates in string"
    else
      aux (i + 1)
  in
  aux 0
;;

let () = Stdio.printf "ind: %d\n" (findFirstDup b)
(* in  *)

(* 
- split the input by char 
- funtion to group 4 items staring in idx
  - check if elements are unique probaly using String.mem or List.find_all_dups
    or List.contains dup or find_a_dup
    
- Iterate over the string until the funtion returns true and escape the idx

*)
