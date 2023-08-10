open Base

type buf = {
  c: int;
  v: int;
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

let doInstr main_b instruction =
  let rec cycle i v st_buf tv =
    let c = main_b.c + i in
    let x = main_b.v + main_b.b in
    let s = strenth c x st_buf in
    let n_tv = drawPixel c x tv in

    if i = 2 then
      { c = main_b.c + 2; v = x; b = v; st = s; crt = n_tv }
    else
      cycle (i + 1) v s n_tv
  in
  match String.split ~on:' ' instruction with
  | ["noop"] ->
    let c = main_b.c + 1 in
    let x = main_b.v + main_b.b in
    let s = strenth c x main_b.st in
    let n_tv = drawPixel c x main_b.crt in

    { c = main_b.c + 1; v = main_b.v + main_b.b; b = 0; st = s; crt = n_tv }
  | ["addx"; v] -> cycle 1 (Int.of_string v) main_b.st main_b.crt
  | _ -> failwith "Bad Input"
;;

let inp = Readfile.read_lines "day10.txt"

let _ =
  let solve =
    List.fold inp ~init:{ c = 0; v = 1; b = 0; st = 0; crt = [""] } ~f:(fun acc inst ->
      doInstr acc inst)
  in
  Stdio.printf "Part1: %d\n" solve.st;
  Readfile.print_listof_strs ~txt:"Part2:\n" (List.rev solve.crt)
;;
