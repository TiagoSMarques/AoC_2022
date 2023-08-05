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
    match String.to_list x with
    | [dir; ' '; mag] -> dir, Char.get_digit_exn mag
    | _ -> failwith "Bad input format")
;;

let moveHd c_pos = function
  | 'U' -> { x = c_pos.x; y = c_pos.y + 1 }
  | 'D' -> { x = c_pos.x; y = c_pos.y - 1 }
  | 'R' -> { x = c_pos.x + 1; y = c_pos.y }
  | 'L' -> { x = c_pos.x - 1; y = c_pos.y }
  | _ -> failwith "unkown direction"
;;

let moveTl pos_tl pos_hd prev_hd =
  (* Right *)
  if pos_hd.x - pos_tl.x >= 2 && pos_hd.y = pos_tl.y then
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
  (* Overlap *)
  else if pos_hd.x = pos_tl.x && pos_hd.y = pos_tl.y then
    { x = pos_tl.x; y = pos_tl.y }
  (* Diagonals *)
  else
    { x = prev_hd.x; y = prev_hd.y }
;;

let makeMove rope (d, mag) =
  let pos_hd, pos_tl = rope.hd, rope.tl in
  let rec loop hd tl i =
    if i >= mag then
      { hd; tl }
    else (
      let new_hd = moveHd hd d in
      let new_tl = moveTl tl new_hd hd in
      loop new_hd new_tl (i + 1)
    )
  in
  loop pos_hd pos_tl 0
;;

(* let inp = Readfile.read_lines "day9.txt" *)
let i = ["R 4"; "U 4"; "L 3"; "D 1"; "R 4"; "D 1"; "L 5"; "R 2"]
let a = parse i
let start = { hd = { x = 0; y = 0 }; tl = { x = 0; y = 0 } }

let b =
  List.fold a ~init:[start] ~f:(fun acc (d, mag) ->
    makeMove (List.hd_exn acc) (d, mag) :: acc)
;;

(* let c = makeMove { hd = { x = 4; y = 0 }; tl = { x = 3; y = 0 } } ('U', 4) *)
