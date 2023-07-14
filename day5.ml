open Base

let inp = Readfile.read_lines "day5.txt"

(* open Str *)

let storage, moves =
  let groups = List.group inp ~break:(fun _ s -> String.compare s "" = 0) in
  match groups with
  | storage' :: moves' :: _ -> storage', List.tl_exn moves'
  | _ -> failwith "Wrong format - no separation"
;;

let t = "move 1 from 2 to 1"
(* Readfile.print_listof_strs a *)

(* If we are sure of the output  *)
let movefmt_2 move_line =
  let map_sp f f2 (a, b, c) l = f (f2 l a), f (f2 l b), f (f2 l c) in
  (* move m from f to t  *)
  map_sp Char.get_digit_exn List.nth_exn (5, 12, 17) (String.to_list move_line)
;;

let movefmt move_line =
  match Str.(split (regexp "\\b[a-z]+\\b") move_line) with
  (* move m from f to t  *)
  | [m; f; t] -> Int.of_string m, Int.of_string f, Int.of_string t
  | []
  | _ ->
    failwith "Wrong format"
;;

let print_queue q = Queue.iter q ~f:(fun element -> Stdio.printf "%d \n" element)
let print_stack q = Stack.iter q ~f:(fun element -> Stdio.printf "%d \n" element)
let a = List.append [1; 4] [6]
let () = Readfile.print_listof_ints a
let my_queue = Queue.create ()
let () = Queue.enqueue my_queue 2
let my_stack = Stack.create ()
let () = Stack.push my_stack 4;;

print_queue my_queue;;
print_stack my_stack

(* format the colum storage in to queues *)
