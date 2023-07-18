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

let makeStacks storage' n_piles =
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

let moveFormat move_line =
  match String.split_on_chars ~on:[' '] move_line with
  | [_; m; "from"; f; "to"; t] ->
    Int.of_string m, Int.of_string f - 1, Int.of_string t - 1
  | []
   |_ ->
    failwith "Wrong format"
;;

let makeMove (m, f, t) st_sto =
  (* let tmp = ref [] in *)
  let m' = ref m in
  while !m' >= 1 do
    Stack.push st_sto.(t) (Stack.pop_exn st_sto.(f));
    Int.decr m'
  done
;;

let part1 inp =
  let storage, n_piles, moves = parseInput inp in
  let stacked_storage_arr = makeStacks storage n_piles in
  let () =
    List.iter moves ~f:(fun move -> makeMove (moveFormat move) stacked_storage_arr)
  in
  Array.iter stacked_storage_arr ~f:(fun x -> Stdio.printf "%c" (Stack.top_exn x))
;;

(* Part 1 *)
let inp = Readfile.read_lines "day5.txt"
let () = part1 inp
