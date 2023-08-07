open Base

type buf = {
  c: int;
  v: int;
  b: int;
}

let check x =
  if x = 20 || x = 60 || x = 100 || x = 140 || x = 180 || x = 220 then
    true
  else
    false
;;

let doInstr main_b instruction =
  let rec cycle buffer =
    if buffer.c = 2 then
      { c = main_b.c + buffer.c; v = main_b.v + buffer.b; b = buffer.v }
    else
      cycle { c = buffer.c + 1; v = buffer.v; b = buffer.b }
  in
  match String.split ~on:' ' instruction with
  | ["noop"] -> { c = main_b.c + 1; v = main_b.v + main_b.b; b = 0 }
  | ["addx"; v] -> cycle { c = 0; v = Int.of_string v; b = main_b.b }
  | _ -> failwith "Bad Input"
;;

let b = doInstr { c = 5; v = 4; b = -5 } "noop"
let inp = Readfile.read_lines "day10.txt"
(* let i = ["noop"; "addx 3"; "addx -5"] *)

let _ =
  List.fold inp ~init:{ c = 0; v = 1; b = 0 } ~f:(fun acc inst ->
    let a = doInstr acc inst in
    Stdio.printf "%d-%d\n" a.c a.v;
    doInstr acc inst)
;;
