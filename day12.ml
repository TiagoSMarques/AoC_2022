open Base
open Bigarray

type 'a tree =
  | Node of 'a * 'a tree * 'a tree * 'a tree * 'a tree
  | Leaf

type dirs = {
  u: bool;
  d: bool;
  l: bool;
  r: bool;
}

let t = Node (1, Node (2, Leaf, Leaf, Leaf, Leaf), Leaf, Leaf, Leaf)

let i = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"

(* let input = Array.of_list @@ Readfile.read_lines "day12.txt" *)
let input = Array.of_list @@ String.split_lines i
let chars_matrix = Array.map input ~f:(fun e -> String.to_array e)

let int_matrix =
  Array2.of_array Int c_layout
  @@ Array.map ~f:(Array.map ~f:(fun c -> Char.to_int c - 97)) chars_matrix
;;

let h, w = Array2.(dim1 int_matrix, dim2 int_matrix)

type pos = {
  x: int;
  y: int;
}

let s = ref { x = 0; y = 0 }
let f = ref { x = 0; y = 0 };;

for x = 0 to w - 1 do
  for y = 0 to h - 1 do
    let curr = int_matrix.{x, y} in
    if curr = -14 then s := { x; y };
    if curr = -28 then f := { x; y }
  done
done

let checkHeight (x, y) curr =
  let is_valid_pos = function
    | 'u' -> y > 0 && curr - int_matrix.{x, y - 1} = 1
    | 'd' -> y < h - 1 && curr - int_matrix.{x, y + 1} = 1
    | 'l' -> x > 0 && curr - int_matrix.{x - 1, y} = 1
    | 'r' -> x < w - 1 && curr - int_matrix.{x + 1, y} = 1
    | _ -> false
  in
  {
    u = is_valid_pos 'u';
    d = is_valid_pos 'd';
    l = is_valid_pos 'l';
    r = is_valid_pos 'r';
  }
;;

let buildTree (x, y) =
  let curr = int_matrix.{x, y} in
  Node (curr, Leaf, Leaf, Leaf, Leaf)
;;

(* E position {2,5} *)
(* let _ = chars_matrix.{2, 5} *)
