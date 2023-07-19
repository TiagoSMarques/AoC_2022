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
  let storeRow row st_stack =
    Array.iteri st_stack ~f:(fun n_th _ ->
      match String.get row (1 + (4 * n_th)) with
      | s when Char.( = ) s ' ' -> ()
      | s -> Stack.push st_stack.(n_th) s)
  in
  let stkd_sto = Array.init n_piles ~f:(fun _ -> Stack.create ()) in
  List.iter storage' ~f:(fun row -> storeRow row stkd_sto);
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

let makeMove_9000 (m, f, t) st_sto =
  for _i = 1 to m do
    Stack.push st_sto.(t) (Stack.pop_exn st_sto.(f))
  done
;;

let makeMove_9001 (m, f, t) st_sto =
  let tmp = Stack.create () in
  for _i = 1 to m do
    Stack.push tmp (Stack.pop_exn st_sto.(f))
  done;
  Stack.iter tmp ~f:(fun x -> Stack.push st_sto.(t) x)
;;

let solve f inp =
  let storage, n_piles, moves = parseInput inp in
  let stacked_storage_arr = makeStacks storage n_piles in
  let () = List.iter moves ~f:(fun move -> f (moveFormat move) stacked_storage_arr) in
  Array.iter stacked_storage_arr ~f:(fun x -> Stdio.printf "%c" (Stack.top_exn x));
  Stdio.printf "\n"
;;

let inp = Readfile.read_lines "day5.txt"

(* Part 1 *)
let () = solve makeMove_9000 inp

(* Part 2 *)
let () = solve makeMove_9001 inp
