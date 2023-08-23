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
      List.map ~f:(fun x -> Int.of_string x)
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

let quot = 
  Map.fold init_bM ~init:1 ~f:(fun ~key:_ ~data:m quot -> let divis, _ ,_ = m.test in quot* divis)
;;

let isOld it = function
  | "old" -> it
  | x -> Int.of_string x
;;

let isP2 = function
  | false ->  3
  | true ->  1
;;

let makePlay monkey p =
  List.map monkey.items ~f:(fun wl ->
    let n_worry_l =
      match monkey.op with
      | [v1; "*"; v2] -> isOld wl v1 * isOld wl v2 % quot / isP2 p
      | [v1; "+"; v2] -> (isOld wl v1 + isOld wl v2) % quot / isP2 p
      | _ -> failwith "Bad parse operation"
    in
    let tt, t, f = monkey.test in
    let dest_monkey =
      if n_worry_l %  tt = 0 then
        t
      else
        f
    in
    n_worry_l, dest_monkey)
;;

let playRound board_Map p =
  let rec updateMonkeys bM k'_list =
    match k'_list with
    | [] -> bM
    | k :: tl ->
      let updates = (0, k) :: makePlay (Map.find_exn bM k) p in
      let n_bM =
        List.fold updates ~init:bM ~f:(fun acc' (n_wl, d_m) ->
          Map.update acc' d_m ~f:(fun v ->
            match v with
            | None -> failwith "No monkey id to update"
            | Some v ->
              if d_m = k then
                {
                  id = v.id;
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
            ))
      in
      updateMonkeys n_bM tl

  in
  updateMonkeys board_Map @@ Map.keys board_Map
;;

let solve p r =
  List.sort ~compare:Int.descending
  @@ List.map ~f:(fun a -> a.n_insp)
  @@ Map.data
  @@ List.fold (List.range 0 r) ~init:init_bM ~f:(fun acc _ -> playRound acc p)
;;

let p1 = solve false 20
let p2 = solve true 10000
let ans1 = List.nth_exn p1 0 * List.nth_exn p1 1
let ans2 = List.nth_exn p2 0 * List.nth_exn p2 1
let _ = Stdio.printf "Part1: %d \nPart2: %d \n" ans1 ans2