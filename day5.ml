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
  match Str.(split (regexp "\\b[a-z]+\\b") move_line) with
  (* move m from f to t  *)
  | [m; f; t] -> Int.of_string m, Int.of_string f, Int.of_string t
  | []
  | _ ->
    failwith "Wrong format"
;;

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

(* let makeStacks storage' =   *)
let inp = Readfile.read_lines "day5.txt"
let storage, n_piles, moves = parseInput inp
let stacked_storage = makeStacks storage n_piles
let stacked_storage_arr = makeStacks_Array storage n_piles

(* let makeMove (m, f, t) st = m + f + t *)

(* let a, b, c = movefmt (List.last_exn moves) *)
(* let t = "move 1 from 2 to 1"
let () = Readfile.print_listof_strs storage ~txt:"Storage: "
let () = Stdio.printf "Piles: %d: \n" n_piles
let () = Readfile.print_listof_strs moves ~txt:"Moves: " *)
let () = Stack.iter ~f:(Stdio.printf "%c ") (List.nth_exn stacked_storage 0)
let () = Stack.iter ~f:(Stdio.printf "%c ") stacked_storage_arr.(0)
(* format the colum storage in to queues *)
