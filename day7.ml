open Base

type fileTree =
  | File of (string * int)
  | Dir of (string * fileTree list)

let appendFiles file = function
  | Dir (n, c) -> Dir (n, file :: c)
  | File _ -> failwith "dir must be a folder"
;;

let remove_first = function
  | [] -> []
  | _ :: tl -> tl
;;

(* Format the input to build the fileTree*)
let rec findAndAppend dir path line =
  let rec aux dir path comp_dir =
    match dir with
    | [] -> failwith ("No dir found" ^ List.hd_exn path)
    | Dir (n, c) :: _ when String.equal n (List.hd_exn path) ->
      if List.length path <= 1 then
        Dir (n, line :: c)
      else
        aux c (remove_first path) (Dir (n, c) :: comp_dir)
    | _ :: tl -> aux tl path comp_dir
  in
  aux dir path []
;;

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

let ex4 = [Dir ("/", [File ("1", 1); Dir ("a", [File ("2", 2)])])]
let ex2 = Dir ("/", [])
let path = ["/"; "a"]
let c = findAndAppend ex4 path (File ("3", 3))
let ex3 = appendFiles (File ("1", 1)) ex2 |> appendFiles (Dir ("a", []))
let inp = Readfile.read_lines "day7.txt"
let a = buildFileTree inp

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
