open Base

type monkey = {
  id: int;
  items: string list;
  op: string list;
  test: string * string * string;
}

let a =
  "Monkey 0: 
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
      List.filter_mapi ~f:(fun i a ->
        match i with
        | 4 | 6 -> Some a
        | _ -> None)
      @@ String.split_on_chars ~on:[' '; ','] l2
    in
    let op =
      List.filteri ~f:(fun i _ -> i = 6 || i = 7 || i = 8)
      @@ String.split_on_chars ~on:['='; ' '] l3
    in
    let test =
      ( List.nth_exn (String.split ~on:' ' l4) 5,
        List.nth_exn (String.split ~on:' ' l5) 9,
        List.nth_exn (String.split ~on:' ' l6) 9 )
    in
    { id; items; op; test }
  | _ -> failwith "Bad input"
;;

(* let p = parse a *)

let board = List.map ~f:(fun a -> parse a) (Readfile.read_paragraph "day11.txt")
(* Parse the input for each monkey - done *)
(* Build monkey list using List.map on the input *)
(* make the play function? *)
(* build the game board function that updates the monkey items base on the play *)
