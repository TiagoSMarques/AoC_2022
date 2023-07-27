open Base

let noDups str =
  let sl = String.length str in
  let rec aux i =
    if String.count str ~f:(fun c2 -> Char.( = ) str.[i] c2) > 1 then
      false
    (* Found a dup *)
    else if i >= sl - 1 then
      true
    (*"No duplicates in string" *)
    else
      aux (i + 1)
  in
  aux 0
;;

let searchString str n =
  let sl = String.length str in
  let rec firstUniquei idx =
    let sub_string = String.sub str ~pos:idx ~len:n in
    if noDups sub_string then
      n + idx
    else if idx >= sl - n then
      failwith "No uniques"
    else
      firstUniquei (idx + 1)
  in
  firstUniquei 0
;;

let inp = Readfile.read_lines "day6.txt"
let () = searchString (List.hd_exn inp) 4 |> Stdio.printf "Part 1: %d\n"
let () = searchString (List.hd_exn inp) 14 |> Stdio.printf "Part 2: %d\n"
