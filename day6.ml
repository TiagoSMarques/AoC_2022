open Base

let inp = Readfile.read_lines "day6.txt"
let b = "asdfdif"

let findFirstDup str =
  let rec aux i =
    if String.count str ~f:(fun c2 -> Char.( = ) str.[i] c2) > 1 && i <= String.length str
    then
      (* Stdio.printf "ind: %d\n" i; *)
      i
    else
      (* Stdio.printf "ind rec: %d\n" (i + 1); *)
      aux (i + 1)
  in
  aux 0
;;

(* in  *)

(* 
- split the input by char 
- funtion to group 4 items staring in idx
  - check if elements are unique probaly using String.mem or List.find_all_dups
    or List.contains dup or find_a_dup
    
- Iterate over the string until the funtion returns true and escape the idx

*)
