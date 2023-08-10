open Base

type buf = {
  c: int;
  v: int;
  b: int;
  st: int;
}

let check x =
  if x = 20 || x = 60 || x = 100 || x = 140 || x = 180 || x = 220 then
    true
  else
    false
;;

let strenth c x st =
  if check c then
    st + (c * x)
  else
    st
;;

let doInstr main_b instruction =
  let rec cycle i v st_buf =
    let s = strenth (main_b.c + i) (main_b.v + main_b.b) st_buf in
    (* Stdio.printf "--%d,%d %d\n" (main_b.c + i) (main_b.v + main_b.b) s; *)
    if i = 2 then
      { c = main_b.c + 2; v = main_b.v + main_b.b; b = v; st = s }
    else
      cycle (i + 1) v s
  in
  match String.split ~on:' ' instruction with
  | ["noop"] ->
    let s = strenth (main_b.c + 1) (main_b.v + main_b.b) main_b.st in
    (* Stdio.printf "--%d,%d %d\n" (main_b.c + 1) (main_b.v + main_b.b) s; *)
    { c = main_b.c + 1; v = main_b.v + main_b.b; b = 0; st = s }
  | ["addx"; v] -> cycle 1 (Int.of_string v) main_b.st
  | _ -> failwith "Bad Input"
;;

let inp = Readfile.read_lines "day10.txt"

let _ =
  let p1 =
    List.fold inp ~init:{ c = 0; v = 1; b = 0; st = 0 } ~f:(fun acc inst ->
      doInstr acc inst)
  in
  Stdio.printf "Part1: %d\n" p1.st
;;