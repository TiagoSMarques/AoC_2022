open Base

type fileTree =
  | File of int
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
        fs.files <- line :: fs.files
      else
        aux fs.files (List.tl_exn path)
    | _ :: tl -> aux tl path
  in
  aux [dir] path;
  dir
;;

(* Not a very good implementation since it doesnt check if the cd command actually enters a valid dir - the code will only fail when appending the results of ls since the path wont be valid *)
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
       | [size; _] -> aux tl (findAndAppend fs path (File (Int.of_string size))) path
       | _ -> failwith "bad format of input")
  in

  aux inp (Dir { name = "/"; files = [] }) []
;;

let sumSubDir file_tree =
  let sum_lst = ref [] in
  let rec aux = function
    | File size -> size
    | Dir fs ->
      let total = List.fold fs.files ~init:0 ~f:(fun acc elm -> aux elm + acc) in
      sum_lst := total :: !sum_lst;
      total
  in
  let _ = aux file_tree in
  !sum_lst
;;

let sizeList = Readfile.read_lines "day7.txt" |> buildFileTree |> sumSubDir

let part1 =
  List.fold sizeList ~init:0 ~f:(fun acc x ->
    if x <= 100_000 then
      x + acc
    else
      acc)
;;

let part2 =
  let max_space = 70_000_000 in
  let res_space = 30_000_000 in
  let free = max_space - List.hd_exn sizeList in
  if Int.is_positive free then
    List.sort sizeList ~compare:ascending
    |> List.find_exn ~f:(fun x -> x + free >= res_space)
  else
    0
;;

Stdio.printf "Part 1: %d\n" part1;;
Stdio.printf "Part 2: %d\n" part2
