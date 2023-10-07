open Base

type monkey = {
  id: int;
  items: int64 list;
  op: string list;
  test: int * int * int;
  n_insp: int;
}

let parse m_string =
  match String.split ~on:'\n' m_string with
  | [l1; l2; l3; l4; l5; l6] ->
    let id = Int.of_string @@ List.nth_exn (String.split_on_chars ~on:[' '; ':'] l1) 1 in
    let items : int64 list =
      List.map ~f:(fun x -> Int64.of_string x)
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
  | x -> Int64.of_string x
;;

let isP2 = function
  | false -> Int64.of_int_exn 3
  | true -> Int64.of_int_exn 1
;;

let makePlay monkey p =
  let open Int64 in
  (* Readfile.print_listof_ints monkey.items; *)
  List.map monkey.items ~f:(fun wl ->
    let n_worry_l : int64 =
      match monkey.op with
      | [v1; "*"; v2] -> isOld wl v1 * isOld wl v2 / isP2 p
      | [v1; "+"; v2] -> (isOld wl v1 + isOld wl v2) / isP2 p
      | _ -> failwith "Bad parse operation"
    in
    let tt, t, f = monkey.test in
    let dest_monkey =
      if n_worry_l % of_int_exn tt = zero then
        t
      else
        f
    in
    n_worry_l, dest_monkey)
;;

let playRound board_Map p =
  let rec updateMonkeys bM k'_list =
    match k'_list with
    | k :: tl ->
      let updates = (Int64.zero, k) :: makePlay (Map.find_exn bM k) p in
      let n_bM =
        List.fold updates ~init:bM ~f:(fun acc' (n_wl, d_m) ->
          Map.update acc' d_m ~f:(fun v ->
            (* Stdio.printf "Value: %Ld Dest: %d \n" n_wl d_m; *)
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

let solve p r =
  List.sort ~compare:Int.descending
  @@ List.map ~f:(fun a -> a.n_insp)
  @@ Map.data
  @@ List.fold (List.range 0 r) ~init:init_bM ~f:(fun acc _ -> playRound acc p)
;;

let p2' =
  List.fold (List.range 0 10000) ~init:init_bM ~f:(fun acc _ -> playRound acc true)
;;

let _ =
  List.iteri ~f:(fun i a -> Readfile.print_listof_ints ~txt:(Int.to_string i) a)
  @@ List.map ~f:(fun a -> a.items)
  @@ Map.data p2'
;;

(* let p2' = List.fold (List.range 0 20) ~init:init_bM ~f:(fun acc _ -> playRound acc true) *)

let _ =
  List.iteri ~f:(fun i a -> Stdio.printf "id: %d ins:%d \n" i a.n_insp) @@ Map.data p2'
;;

let p1 = solve false 20
let p2 = solve true 10000
let ans1 = List.nth_exn p1 0 * List.nth_exn p1 1
let ans2 = List.nth_exn p2 0 * List.nth_exn p2 1
let _ = Stdio.printf "Part1: %d \n Part2: %d \n" ans1 ans2
