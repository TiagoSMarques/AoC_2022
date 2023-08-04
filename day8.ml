open Base
open Bigarray

let parseToTree data =
  let makeTree e = Char.get_digit_exn e in
  let makeRow row = Array.map ~f:makeTree (String.to_array row) in
  Array.map ~f:makeRow (List.to_array data)
;;

let isVisible table coord (h, w) =
  let x, y = coord in
  let c_height = table.{x, y} in

  let checkUp =
    let visible = ref (-1) in
    for i = x - 1 downto 0 do
      if table.{i, y} < c_height && not (!visible = 0) then
        visible := 1
      else
        visible := 0
    done;
    !visible = 1
  in
  let checkDown =
    let visible = ref (-1) in
    for i = x + 1 to h - 1 do
      if table.{i, y} < c_height && not (!visible = 0) then
        visible := 1
      else
        visible := 0
    done;
    !visible = 1
  in
  let checkLeft =
    let visible = ref (-1) in
    for i = y - 1 downto 0 do
      if table.{x, i} < c_height && not (!visible = 0) then
        visible := 1
      else
        visible := 0
    done;
    !visible = 1
  in
  let checkRight =
    let visible = ref (-1) in
    for i = y + 1 to w - 1 do
      if table.{x, i} < c_height && not (!visible = 0) then
        visible := 1
      else
        visible := 0
    done;
    !visible = 1
  in
  checkUp || checkDown || checkLeft || checkRight
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

Stdio.printf "\n Part 1: %d" part1
