open Base

let parseInput inp =
  let numberOfPiles piles' =
    let l = String.length piles' in
    Char.get_digit_exn (String.get piles' (l - 2))
  in
  let groups = List.group inp ~break:(fun _ s -> String.compare s "" = 0) in
  match groups with
  | storage' :: moves' :: _ ->
    ( List.rev (List.drop_last_exn storage'),
      numberOfPiles (List.last_exn storage'),
      List.tl_exn moves' )
  | _ -> failwith "Wrong format - no separation"
;;

let moveFormat move_line =
  (* "\\b[a-z]+\\b" *)
  match Str.(split (regexp "\\b[a-z]+\\b") move_line) with
  (* move m from f to t  *)
  | [m; f; t] -> m, f, t
  | []
  | _ ->
    failwith "Wrong format"
;;

Readfile.print_listof_strs
  Str.(split (regexp "{|move \[a-z] from \[a-z] to \[a-z]|}") "move 1 from 2 to 1")

;;
(* let () = Stdio.printf "%s" (Str.regexp {|hello \([A-Za-z]+\)|} in
Str.replace_first r {|\1|} "hello world")  *)
(* let moveFormat move_line =
  let map_sp f f2 (a, b, c) l = f (f2 l a), f (f2 l b) - 1, f (f2 l c) - 1 in
  (* move m from f to t  *)
  map_sp Char.get_digit_exn String.get (5, 12, 17) move_line
;; *)

let makeStacks storage' n_piles =
  let storeRow row n st_stack =
    for n_th = 0 to n - 1 do
      let getChar = String.get row (1 + (4 * n_th)) in
      if not (Char.( = ) getChar ' ') then Stack.push (List.nth_exn st_stack n_th) getChar
    done
  in
  let stkd_sto = List.init n_piles ~f:(fun _ -> Stack.create ()) in
  List.iter storage' ~f:(fun row -> storeRow row n_piles stkd_sto);
  stkd_sto
;;

let makeStacks_Array storage' n_piles =
  let storeRow row n st_stack =
    for n_th = 0 to n - 1 do
      let getChar = String.get row (1 + (4 * n_th)) in
      if not (Char.( = ) getChar ' ') then Stack.push st_stack.(n_th) getChar
    done
  in
  let stkd_sto = Array.init n_piles ~f:(fun _ -> Stack.create ()) in
  List.iter storage' ~f:(fun row -> storeRow row n_piles stkd_sto);
  stkd_sto
;;

let rec doNtimes f e n =
  if n <= 0 then
    ()
  else
    doNtimes f e (n - 1)
;;

let makeMove (m, f, t) st_sto =
  (* let tmp = ref [] in *)
  let m' = ref m in
  while !m' >= 1 do
    Stack.push st_sto.(t) (Stack.pop_exn st_sto.(f));
    Int.decr m'
  done
;;

(* let makeStacks storage' =   *)
let inp = Readfile.read_lines "day5.txt"
let storage, n_piles, moves = parseInput inp
let stacked_storage = makeStacks storage n_piles
let stacked_storage_arr = makeStacks_Array storage n_piles

(* doNtimes (Stack.push st_sto.(t)) (Stack.pop_exn st_sto.(f)) (m-1) *)
(* for i = 1 to m do
    let () = Stdio.printf "%d" i in
    Stack.push st_sto.(t) (Stack.pop_exn st_sto.(f)) 
  done; *)
(* let () = Readfile.print_listof_chars ~txt:"kkkk" !tmp in *)
(* List.iter !tmp ~f:(fun e -> Stack.push st_sto.(t) e)  *)

(* let tmp = doNtimes (Stack.pop_exn st_sto.(f)) [] m in
  let () = Readfile.print_listof_chars ~txt:"kkkk" tmp in
  List.iter tmp ~f:(fun e -> Stack.push st_sto.(t) e) *)

(* Stdio.printf "lala %d %d %d\n" m f t *)

(* makeMove (1, 2 - 1, 1 - 1) stacked_storage_arr *)
(* let () = List.iter moves ~f:(fun move -> makeMove (moveFormat move) stacked_storage_arr) *)

(* let a, b, c = movefmt (List.last_exn moves) *)
(* let t = "move 1 from 2 to 1"
let () = Readfile.print_listof_strs storage ~txt:"Storage: "
let () = Stdio.printf "Piles: %d: \n" n_piles

let () = Readfile.print_listof_strs moves ~txt:"Moves: " *)

let a, b, c = moveFormat (List.nth_exn moves 0)
let () = Stdio.printf "-%s-%s-%s-\n" a b c

(* let () = makeMove (a, b, c) stacked_storage_arr in

(* let () =
  List.iter moves ~f:(fun move ->
    let a, b, c = moveFormat move in
    Stdio.printf "%d %d %d" a b c)
;; *)
let () = Stdio.printf "\n"
let () = Stack.iter ~f:(Stdio.printf "%c") stacked_storage_arr.(0)
let () = Stdio.printf "\n"
let () = Stack.iter ~f:(Stdio.printf "%c") stacked_storage_arr.(1)
let () = Stdio.printf "\n"
let () = Stack.iter ~f:(Stdio.printf "%c") stacked_storage_arr.(2)
let () = Stdio.printf "\n"
(* format the colum storage in to queues *)*)
