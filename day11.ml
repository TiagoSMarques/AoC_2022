open Base

type monkey = {
  id: int;
  items: int list;
  op: string;
  testT: string;
  testF: string;
}

let a =
  [
    "Monkey 0:
Starting items: 79, 98
Operation: new = old * 19
Test: divisible by 23
  If true: throw to monkey 2
  If false: throw to monkey 3";
  ]
;;

let b = Readfile.read_paragraph "day11.txt"
let _ = Stdio.printf "%s" (List.hd_exn b)

(* Parse the input for each monkey *)
(* Build monkey list using List.map on the input *)
