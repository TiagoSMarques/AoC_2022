open Base

type monkey = {
  id: int;
  items: int list;
  op: string list;
  test: int * int * int;
  n_insp: int;
}

let a =
  "Monkey 1:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3"
;;

let parse m_string =
  match String.split ~on:'\n' m_string with
  | [l1; l2; l3; l4; l5; l6] ->
    let id = Int.of_string @@ List.nth_exn (String.split_on_chars ~on:[' '; ':'] l1) 1 in
    let items =
      List.map ~f:Int.of_string
      @@ List.filteri ~f:(fun i _ -> i >= 4 && ((2 * i) + 4) % 4 = 0)
      @@ String.split_on_chars ~on:[' '; ','] l2
    in
    let op =
      List.filteri ~f:(fun i _ -> i = 6 || i = 7 || i = 8)
      @@ String.split_on_chars ~on:['='; ' '] l3
    in
    let test =
      ( Int.of_string @@ List.nth_exn (String.split ~on:' ' l4) 5,
        Int.of_string @@ List.nth_exn (String.split ~on:' ' l5) 9,
        Int.of_string @@ List.nth_exn (String.split ~on:' ' l6) 9 )
    in
    { id; items; op; test; n_insp = 0 }
  | _ -> failwith "Bad input"
;;

let init_bM =
  List.fold
    ~init:(Map.empty (module Int))
    ~f:(fun acc elem ->
      let m = parse elem in
      let k = m.id in
      Map.add_exn acc ~key:k ~data:m)
  @@ Readfile.read_paragraph "day11.txt"
;;

let isOld it = function
  | "old" -> it
  | x -> Int.of_string x
;;

let makePlay monkey =
  (* Readfile.print_listof_ints monkey.items; *)
  List.map monkey.items ~f:(fun wl ->
    let n_worry_l =
      match monkey.op with
      | [v1; "*"; v2] -> isOld wl v1 * isOld wl v2 / 3
      | [v1; "+"; v2] -> (isOld wl v1 + isOld wl v2) / 3
      | _ -> failwith "Bad parse operation"
    in
    let tt, t, f = monkey.test in
    let dest_monkey =
      if n_worry_l % tt = 0 then
        t
      else
        f
    in
    n_worry_l, dest_monkey)
;;

let playRound board_Map =
  let rec updateMonkeys bM k'_list =
    match k'_list with
    | k :: tl ->
      let updates = (0, k) :: makePlay (Map.find_exn bM k) in
      let n_bM =
        List.fold updates ~init:bM ~f:(fun acc' (n_wl, d_m) ->
          Map.update acc' d_m ~f:(fun v ->
            (* Stdio.printf "Value: %d Dest: %d \n" n_wl d_m; *)
            match v with
            | Some v ->
              if d_m = k then
                {
                  id = k;
                  items = [];
                  op = v.op;
                  test = v.test;
                  n_insp = List.length updates - 1 + v.n_insp;
                }
              else
                {
                  id = v.id;
                  items = n_wl :: v.items;
                  op = v.op;
                  test = v.test;
                  n_insp = v.n_insp;
                }
            | None -> failwith "No monkey id to update"))
      in
      updateMonkeys n_bM tl
    | [] -> bM
  in
  updateMonkeys board_Map @@ Map.keys board_Map
;;

(* let m = playRound init_bM *)
let d = List.fold (List.range 0 20) ~init:init_bM ~f:(fun acc _ -> playRound acc)
let v = List.sort ~compare:Int.descending @@ List.map ~f:(fun a -> a.n_insp) @@ Map.data d
let p1 = List.nth_exn v 0 * List.nth_exn v 1
let _ = Stdio.printf "%d" p1

(* let _ = *)
(*   List.iteri ~f:(fun i a -> Readfile.print_listof_ints ~txt:(Int.to_string i) a) *)
(*   @@ List.map ~f:(fun a -> a.items) *)
(*   @@ Map.data d *)
(* ;; *)
(**)
(* Parse the input for each monkey - done *)
(* Build monkey list using List.map on the input *)
(* make the play function? *)
(* build the game board function that updates the monkey items base on the play *)
