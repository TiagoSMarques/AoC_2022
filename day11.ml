open Base

type monkey = {
  id: int;
  items: int list;
  op: string list;
  test: int * int * int;
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
    { id; items; op; test }
  | _ -> failwith "Bad input"
;;

let c = parse a

let init_board_Map =
  List.fold
    ~init:(Map.empty (module Int))
    ~f:(fun acc elem ->
      let m = parse elem in
      let k = m.id in
      Map.add_exn acc ~key:k ~data:m)
  @@ Readfile.read_paragraph "day11.txt"
;;

let playRound board_Map =
  let isOld it = function
    | "old" -> it
    | x -> Int.of_string x
  in
  let makePlay monkey curr_board =
    let move_list =
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
    in
    (*update current monkey*)
    let up_monkey =
      Map.set
        curr_board
        ~key:monkey.id
        ~data:{ id = monkey.id; items = []; op = monkey.op; test = monkey.test }
    in
    up_monkey, move_list
  in

  Map.fold board_Map ~init:board_Map ~f:(fun ~key:_ ~data:v acc ->
    (*update next monkey*)
    let new_board, updates = makePlay v acc in

    let d =
      List.fold updates ~init:new_board ~f:(fun acc' (n_wl, d_v) ->
        Map.update acc' d_v ~f:(fun v ->
          match v with
          | Some v -> { id = v.id; items = n_wl :: v.items; op = v.op; test = v.test }
          | None -> failwith "No monkey id to update"))
    in
    let _ =
      List.iteri ~f:(fun i a -> Readfile.print_listof_ints ~txt:(Int.to_string i) a)
      @@ List.map ~f:(fun a -> a.items)
      @@ Map.data d
    in
    d)
;;

let d = playRound init_board_Map

(* Parse the input for each monkey - done *)
(* Build monkey list using List.map on the input *)
(* make the play function? *)
(* build the game board function that updates the monkey items base on the play *)
