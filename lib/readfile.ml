open Stdio

let read_lines filename : string list =
  let ic = In_channel.create filename in
  let rec read_lines_aux acc =
    match In_channel.input_line ic with
    | Some line -> read_lines_aux (line :: acc)
    | None ->
        In_channel.close ic;
        List.rev acc
  in
  read_lines_aux []
