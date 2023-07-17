open Stdlib

let read_lines file =
  In_channel.with_open_text file In_channel.input_all |> Str.(split (regexp "\n"))
;;

let print_listof_ints ?(txt = "") ints =
  Format.printf
    "%s %a \n"
    txt
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       Format.pp_print_int)
    ints
;;

let print_listof_strs ?(txt = "") ints =
  Format.printf
    "%s %a \n"
    txt
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       Format.pp_print_string)
    ints
;;

let print_listof_chars ?(txt = "") chars =
  Format.printf
    "%s %a \n"
    txt
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       Format.pp_print_char)
    chars
;;

let split_once ch str =
  let[@ocaml.warning "-8"] [left; right] = String.split_on_char ch str in
  left, right
;;
