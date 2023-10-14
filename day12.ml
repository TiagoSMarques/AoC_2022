open Base
open Bigarray

type 'a tree =
  | Node of 'a * 'a tree * 'a tree * 'a tree * 'a tree
  | Leaf

type dir =
  | U
  | D
  | L
  | R

type pos = {
  y: int;
  x: int;
}

let tuple_equal (a, b) (x, y) = a = x && b = y
let input = Array.of_list @@ Readfile.read_lines "day12.txt"
(* let input = Array.of_list @@ String.split_lines i *)

let int_matrix, start, finish =
  let s = ref { y = 0; x = 0 } in
  let f = ref { y = 0; x = 0 } in

  let matrix =
    Array2.of_array Int c_layout
    @@ Array.mapi input ~f:(fun y e ->
      Array.mapi
        ~f:(fun x v ->
          match v with
          | 'S' ->
            s := { y; x };
            0
          | 'E' ->
            f := { y; x };
            25
          | _ -> Char.to_int v - 97)
        (String.to_array e))
  in
  matrix, !s, !f
;;

let h, w = Array2.(dim1 int_matrix, dim2 int_matrix)

(* fix this - i replaced the values for the S and E above so this no longer works *)

(* let curr = int_matrix.{y, x} in *)
let buildTree m start finish =
  let is_valid_pos (x, y) curr = function
    | U -> y > 0 && abs (int_matrix.{y - 1, x} - curr) <= 1
    | D -> y < h - 1 && abs (int_matrix.{y + 1, x} - curr) <= 1
    | L -> x > 0 && abs (int_matrix.{y, x - 1} - curr) <= 1
    | R -> x < w - 1 && abs (int_matrix.{y, x + 1} - curr) <= 1
  in

  let rec addNode (x, y) visited_pos =
    let curr = m.{y, x} in
    let is_last = x = finish.x && y = finish.y in
    let is_visited = List.mem visited_pos (x, y) ~equal:tuple_equal in
    let checkDir d = (not is_last) && (not is_visited) && is_valid_pos (x, y) curr d in
    let v = if is_last then 100 else curr in
    Node
      ( v,
        (if checkDir U then addNode (x, y - 1) ((x, y) :: visited_pos) else Leaf),
        (if checkDir D then addNode (x, y + 1) ((x, y) :: visited_pos) else Leaf),
        (if checkDir L then addNode (x - 1, y) ((x, y) :: visited_pos) else Leaf),
        if checkDir R then addNode (x + 1, y) ((x, y) :: visited_pos) else Leaf )
  in

  addNode (start.x, start.y) []
;;

let t = buildTree int_matrix start finish

let rec nodeDepth tree depth =
  match tree with
  | Node (v, _, _, _, _) when v = 100 -> Some depth
  | Node (_, t1, t2, t3, t4) ->
    let depths =
      List.fold
        ~f:(fun acc subTree ->
          match nodeDepth subTree (depth + 1) with
          | Some d -> d :: acc
          | None -> acc)
        ~init:[]
        [t1; t2; t3; t4]
    in
    if List.length depths > 0 then List.hd (List.sort ~compare depths) else None
  | Leaf -> None
;;

let dd =
  match nodeDepth t 0 with
  | Some x -> x
  | None -> 0
;;

let _ = Stdio.printf "Part 1: %d\n" dd
