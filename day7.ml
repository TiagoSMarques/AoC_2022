open Base

type fileTree =
  | File of (string * int)
  | Dir of (string * fileTree list)

let appendFiles file = function
  | Dir (n, c) -> Dir (n, file :: c)
  | File _ -> failwith "dir must be a folder"
;;

(* Format the input to build the fileTree*)

let buildFileTree inp =
  let rec aux inp depth =
    match inp with
    | [] -> failwith "done"
    | hd :: tl ->
      (match String.split hd ~on:' ' with
       | ["$"; "cd"; ".."] ->
         Stdio.printf "Up depth %d\n" (depth - 1);
         aux tl (depth - 1)
       | ["$"; "cd"; folder_name] ->
         Stdio.printf "Create folder: %s \n" folder_name;
         aux tl depth
       | ["$"; "ls"] ->
         Stdio.printf "List depth %d \n" (depth + 1);
         aux tl (depth + 1)
       | ["dir"; dir_name] ->
         Stdio.printf "Create folder: %s \n" dir_name;
         aux tl depth
       | [size; file_name] ->
         Stdio.printf "File: %s Size:%s \n" file_name size;
         aux tl depth
       | _ -> failwith "bad format of input")
  in

  aux inp 0
;;

let dictionary = ["a", [File ("1", 1); Dir ("b", [])]; "banana", [File ("3", 3)]]
let updated_dictionary = List.Assoc.add dictionary ~equal:String.equal "a"
let ex4 = Dir ("/", [File ("1", 1); Dir ("a", [File ("2", 2)])])

let app_at_depth dir target_depth name =
  let rec aux depth folder =
    match folder with
    | Dir (_, c) ->
      if depth < target_depth then
        aux (depth + 1) c
      else
        c
    | File _ -> failwith "dir must be a folder"
  in
  aux 0 dir
;;

let inp = Readfile.read_lines "day7.txt"
let a = buildFileTree inp
let ex2 = Dir ("/", [])
let ex3 = appendFiles (File ("1", 1)) ex2 |> appendFiles (Dir ("a", []))

let sumList file_tree =
  let sum_lst = ref [] in
  let rec aux = function
    | File (_, size) -> size
    | Dir (_, n) ->
      let total = List.fold n ~init:0 ~f:(fun acc elm -> aux elm + acc) in
      sum_lst := total :: !sum_lst;
      total
  in
  let _ = aux file_tree in
  !sum_lst
;;

(* let example_dir =
   Dir [File ("1", 1); File ("2", 2); Dir [File ("3", 3); File ("4", 4)]; File ("5", 5)]
   ;; *)

(* let ex2 = Dir [Dir [File ("1", 1)]; Dir [File ("2", 2)]]
   let ex3 = Dir [File ("1", 1); Dir [File ("2", 2)]]
   let ex4 = Dir [Dir [File ("1", 1); Dir [File ("5", 5)]]; Dir [File ("3", 3)]] *)
(* let a = sumList example_dir;; *)

let b = sumList ex2;;

Readfile.print_listof_ints b
