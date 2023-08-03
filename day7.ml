open Base

type fileTree =
  | File of (string * int)
  | Dir of {
      name: string;
      mutable files: fileTree list;
    }

let rec remove_last lst =
  match lst with
  | [] -> []
  | [_] -> []
  | hd :: tl -> hd :: remove_last tl
;;

(* traveses the path until the last folder and appends the line to it *)
let findAndAppend dir path line =
  let rec aux dir path =
    match dir with
    | [] -> failwith ("No dir found: " ^ List.hd_exn path)
    | Dir fs :: _ when String.equal fs.name (List.hd_exn path) ->
      if List.length path <= 1 then
        (* Stdio.printf "folder: %s " fs.name; *)
        fs.files <- line :: fs.files
      else
        aux fs.files (List.tl_exn path)
    | _ :: tl -> aux tl path
  in
  aux [dir] path;
  dir
;;

let buildFileTree inp =
  let rec aux inp fs path =
    match inp with
    | [] -> fs
    | hd :: tl ->
      (match String.split hd ~on:' ' with
       | ["$"; "cd"; "/"] -> aux tl fs ["/"]
       | ["$"; "cd"; ".."] -> aux tl fs (remove_last path)
       | ["$"; "cd"; folder_name] -> aux tl fs (path @ [folder_name])
       | ["$"; "ls"] -> aux tl fs path
       | ["dir"; dir_name] ->
         aux tl (findAndAppend fs path (Dir { name = dir_name; files = [] })) path
       | [size; file_name] ->
         aux tl (findAndAppend fs path (File (file_name, Int.of_string size))) path
       | _ -> failwith "bad format of input")
  in

  aux inp (Dir { name = "/"; files = [] }) []
;;

let inp = Readfile.read_lines "day7.txt"
let a = buildFileTree inp

let sumList file_tree =
  let sum_lst = ref [] in
  let rec aux = function
    | File (_, size) -> size
    | Dir fs ->
      let total = List.fold fs.files ~init:0 ~f:(fun acc elm -> aux elm + acc) in
      sum_lst := total :: !sum_lst;
      total
  in
  let _ = aux file_tree in
  !sum_lst
;;

let b = sumList a

let part1 =
  List.fold b ~init:0 ~f:(fun acc x ->
    if x <= 100_000 then
      x + acc
    else
      acc)
;;

(* Readfile.print_listof_ints b; *)
Stdio.printf "\n Part 1: %d" part1
