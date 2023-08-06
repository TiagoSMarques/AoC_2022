module Point = struct
  type t = int * int

  let compare = compare
end

module PointSet = Set.Make (Point)
open Base

type pos = {
  x: int;
  y: int;
}

type rope = {
  hd: pos;
  tl: pos;
}

let parse inp =
  List.map inp ~f:(fun x ->
    match String.split ~on:' ' x with
    | [dir; mag] -> dir, Int.of_string mag
    | _ -> failwith "Bad input format")
;;

let moveHd c_pos = function
  | "U" -> { x = c_pos.x; y = c_pos.y + 1 }
  | "D" -> { x = c_pos.x; y = c_pos.y - 1 }
  | "R" -> { x = c_pos.x + 1; y = c_pos.y }
  | "L" -> { x = c_pos.x - 1; y = c_pos.y }
  | _ -> failwith "unkown direction"
;;

let moveTl pos_tl pos_hd =
  (* Still close - dont move *)
  if abs (pos_hd.x - pos_tl.x) <= 1 && abs (pos_hd.y - pos_tl.y) <= 1 then
    { x = pos_tl.x; y = pos_tl.y }
  (* Right *)
  else if pos_hd.x - pos_tl.x >= 2 && pos_hd.y = pos_tl.y then
    { x = pos_tl.x + 1; y = pos_tl.y }
  (* Left *)
  else if pos_hd.x - pos_tl.x <= -2 && pos_hd.y = pos_tl.y then
    { x = pos_tl.x - 1; y = pos_tl.y }
  (* Up *)
  else if pos_hd.x = pos_tl.x && pos_hd.y - pos_tl.y >= 2 then
    { x = pos_tl.x; y = pos_tl.y + 1 }
  (* Down *)
  else if pos_hd.x = pos_tl.x && pos_hd.y - pos_tl.y <= -2 then
    { x = pos_tl.x; y = pos_tl.y - 1 }
  (* Diagonals quad_1 *)
  else if pos_hd.x - pos_tl.x >= 1 && pos_hd.y - pos_tl.y >= 1 then
    { x = pos_tl.x + 1; y = pos_tl.y + 1 }
  (* quad_2 *)
  else if pos_hd.x - pos_tl.x <= -1 && pos_hd.y - pos_tl.y >= 1 then
    { x = pos_tl.x - 1; y = pos_tl.y + 1 }
  (* quad_3 *)
  else if pos_hd.x - pos_tl.x <= -1 && pos_hd.y - pos_tl.y <= -1 then
    { x = pos_tl.x - 1; y = pos_tl.y - 1 }
  (* quad_4 *)
  else
    { x = pos_tl.x + 1; y = pos_tl.y - 1 }
;;

let makeMove rope (d, mag) tl_path =
  let rec loop hd tl i path =
    if i >= mag then
      { hd; tl }, path
    else (
      let new_hd = moveHd hd d in
      let new_tl = moveTl tl new_hd in
      let new_path = PointSet.add (new_tl.x, new_tl.y) path in
      loop new_hd new_tl (i + 1) new_path
    )
  in
  loop rope.hd rope.tl 0 tl_path
;;

let makeMove2 rope_list (d, mag) tl_path =
  let rec loop new_lst i path =
    if i >= mag then
      new_lst, path
    else (
      let hd = List.hd_exn new_lst in
      let tl_list = List.tl_exn new_lst in
      let new_hd = moveHd hd d in
      let _, mapped_list =
        List.fold_map tl_list ~init:new_hd ~f:(fun acc x ->
          let new_tl = moveTl x acc in
          new_tl, new_tl)
      in
      let last_tl = List.nth_exn mapped_list 8 in
      let new_path = PointSet.add (last_tl.x, last_tl.y) path in
      loop (new_hd :: mapped_list) (i + 1) new_path
    )
  in
  loop rope_list 0 tl_path
;;

let part1 inp =
  let start = { hd = { x = 0; y = 0 }; tl = { x = 0; y = 0 } } in
  let rec aux prev_rope tl_path = function
    | [] -> PointSet.cardinal tl_path
    | hd :: tl ->
      let new_move, tl_path = makeMove prev_rope hd tl_path in
      aux new_move tl_path tl
  in
  aux start PointSet.empty inp
;;

let part2 inp =
  let start = { x = 0; y = 0 } in
  let knot_list = List.init 10 ~f:(fun _ -> start) in
  let rec aux new_list tl_path = function
    | [] -> PointSet.cardinal tl_path
    | hd :: tl ->
      let new_move, tl_path = makeMove2 new_list hd tl_path in
      aux new_move tl_path tl
  in
  aux knot_list PointSet.empty inp
;;

let _ =
  let move_list = Readfile.read_lines "day9.txt" |> parse in
  Stdio.printf "Part1: %d\n" (part1 move_list);
  Stdio.printf "Part2: %d\n" (part2 move_list)
;;
