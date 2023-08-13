open Base
open Re

type monkey = {
  id: int;
  items: int list;
  op: string list;
  testT: string;
  testF: string;
}

let a =
  "Monkey 0:
Starting items: 79, 98
Operation: new = old * 19
Test: divisible by 23
  If true: throw to monkey 2
  If false: throw to monkey 3"
;;

let b = Array.of_list @@ String.split ~on:'\n' a
let l1 = b.(0)
let id = Int.of_string @@ List.nth_exn (String.split_on_chars ~on:[' '; ':'] l1) 1
let l2 = b.(1)
let matches = Re.exec (Re.Posix.compile_pat "([0-9]+), ([0-9]+)") l2

let id =
  [Int.of_string @@ Re.Group.get matches 1; Int.of_string @@ Re.Group.get matches 2]
;;

let l3 = b.(2)
let matches = Re.exec (Re.Perl.compile_pat "(?=w)[a-zA-Z0-9_]+") l3
let op = List.tl_exn @@ String.split ~on:'=' l3

(*
    
*)
let b = Readfile.read_paragraph "day11.txt"
let _ = Stdio.printf "%s" (List.hd_exn b)

let parse m_string =
  match Str.(split (regexp "\n")) m_string with
  | [l1; l2; l3; l4; l5; l6] -> 2
  | _ -> failwith "Bad input"
;;

(* Parse the input for each monkey *)
(* Build monkey list using List.map on the input *)
