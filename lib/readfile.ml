open Stdlib

let read_lines filename : string list =
  let ic = Stdio.In_channel.create filename in
  let rec read_lines_aux acc =
    match In_channel.input_line ic with
    | Some line -> read_lines_aux (line :: acc)
    | None ->
      In_channel.close ic;
      List.rev acc
  in
  read_lines_aux []
;;

let read_lines_2 file =
  In_channel.with_open_text file In_channel.input_all |> Str.(split (regexp "\n"))
;;
