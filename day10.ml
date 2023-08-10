open Base

type buf = {
  c: int;
  x: int;
  b: int;
  st: int;
  crt: string list;
}

let strenth c x st =
  match c with
  | 20 | 60 | 100 | 140 | 180 | 220 -> st + (c * x)
  | _ -> st
;;

let drawPixel c x crt =
  let line l =
    match c with
    | 40 | 80 | 120 | 160 | 200 -> "" :: l
    | _ -> l
  in
  let n_crt = line crt in
  if (c - 1) % 40 >= x - 1 && (c - 1) % 40 <= (x + 1) % 40 then
    (List.hd_exn n_crt ^ "#") :: List.tl_exn n_crt
  else
    (List.hd_exn n_crt ^ ".") :: List.tl_exn n_crt
;;

let doInstr buffer instruction =
  let rec cycle i b st_buf tv noop_case =
    let c = buffer.c + i in
    let x = buffer.x + buffer.b in
    let st = strenth c x st_buf in
    let crt = drawPixel c x tv in

    if i = 2 || noop_case then
      { c; x; b; st; crt }
    else
      cycle (i + 1) b st crt false
  in
  match String.split ~on:' ' instruction with
  | ["noop"] -> cycle 1 0 buffer.st buffer.crt true
  | ["addx"; v] -> cycle 1 (Int.of_string v) buffer.st buffer.crt false
  | _ -> failwith "Bad Input"
;;

let _ =
  let solve =
    Readfile.read_lines "day10.txt"
    |> List.fold ~init:{ c = 0; x = 1; b = 0; st = 0; crt = [""] } ~f:(fun acc inst ->
      doInstr acc inst)
  in
  Stdio.printf "Part1: %d\n" solve.st;
  Readfile.print_listof_strs ~txt:"Part2:\n" (List.rev solve.crt)
;;
