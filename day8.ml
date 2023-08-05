open Base
open Bigarray

let parseToTree data =
  let makeTree e = Char.get_digit_exn e in
  let makeRow row = Array.map ~f:makeTree (String.to_array row) in
  Array.map ~f:makeRow (List.to_array data)
;;

let isVisible table (x, y) (h, w) =
  let c_height = table.{x, y} in

  let rec check (x1, y1) (dx, dy) =
    if x1 < 0 || x1 >= h || y1 < 0 || y1 >= w then
      true
    else if table.{x1, y1} >= c_height then
      false
    else
      check (x1 + dx, y1 + dy) (dx, dy)
  in
  let checkUp = check (x - 1, y) (-1, 0) in
  let checkDown = check (x + 1, y) (1, 0) in
  let checkLeft = check (x, y - 1) (0, -1) in
  let checkRight = check (x, y + 1) (0, 1) in

  checkUp || checkDown || checkLeft || checkRight
;;

let scenicScore table (x, y) (h, w) =
  let c_height = table.{x, y} in

  let rec check (x1, y1) (dx, dy) score =
    if x1 < 0 || x1 >= h || y1 < 0 || y1 >= w then
      score
    else if table.{x1, y1} >= c_height then
      score + 1
    else
      check (x1 + dx, y1 + dy) (dx, dy) score + 1
  in
  let checkUp = check (x - 1, y) (-1, 0) 0 in
  let checkDown = check (x + 1, y) (1, 0) 0 in
  let checkLeft = check (x, y - 1) (0, -1) 0 in
  let checkRight = check (x, y + 1) (0, 1) 0 in

  checkUp * checkDown * checkLeft * checkRight
;;

let inp = Readfile.read_lines "day8.txt"
let c = Array2.of_array Int c_layout (parseToTree inp)
let h, w = Array2.dim1 c, Array2.dim2 c

let part1 =
  let sum = ref ((2 * h) + (2 * w) - 4) in
  for i = 1 to h - 2 do
    for j = 1 to w - 2 do
      if isVisible c (i, j) (h, w) then sum := !sum + 1
    done
  done;
  !sum
;;

let part2 =
  let score = ref 0 in
  for i = 1 to h - 2 do
    for j = 1 to w - 2 do
      let tree_score = scenicScore c (i, j) (h, w) in
      if tree_score > !score then score := tree_score
    done
  done;
  !score
;;

Stdio.printf "\n Part 1: %d" part1;
Stdio.printf "\n Part 1: %d" part2
