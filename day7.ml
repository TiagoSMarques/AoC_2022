open Base

type fileTree =
  | File of (string * int)
  | Dir of fileTree list

(* Format the input to build the fileTree*)

let inp = Readfile.read_lines "day7.txt"

let buildFileTree inp =
  let rec aux inp depth =
    match inp with
    | [] -> failwith "done"
    | hd :: tl ->
      let instr = String.split hd ~on:' ' in
      (match instr with
       | ["$"; "cd"; ".."] ->
         Stdio.printf "up one depth d:%d\n" (depth - 1);
         aux tl (depth - 1)
       | ["$"; "cd"; fold] ->
         Stdio.printf "create folder: %s \n" fold;
         aux tl depth
       | ["$"; "ls"] ->
         Stdio.printf "List contents d:%d \n" (depth + 1);
         aux tl (depth + 1)
       | _ ->
         Stdio.printf "Resto: \n";
         aux tl depth)
  in

  aux inp 0
;;

let a = buildFileTree inp

let sumList file_tree =
  let sum_lst = ref [] in
  let rec aux = function
    | File (_, size) -> size
    | Dir ft ->
      let total = List.fold ft ~init:0 ~f:(fun acc elm -> aux elm + acc) in
      sum_lst := total :: !sum_lst;
      total
  in
  let _ = aux file_tree in
  !sum_lst
;;

let example_dir =
  Dir [File ("1", 1); File ("2", 2); Dir [File ("3", 3); File ("4", 4)]; File ("5", 5)]
;;

let ex1 = Dir [File ("1", 1); File ("2", 2); File ("3", 3)]
let ex2 = Dir [Dir [File ("1", 1)]; Dir [File ("2", 2)]]
let ex3 = Dir [File ("1", 1); Dir [File ("2", 2)]]
let ex4 = Dir [Dir [File ("1", 1); Dir [File ("5", 5)]]; Dir [File ("3", 3)]]
(* let a = sumList example_dir;; *)

let b = sumList ex4;;

Readfile.print_listof_ints b
