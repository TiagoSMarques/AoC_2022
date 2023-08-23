open Stdlib

let read_lines file =
  In_channel.with_open_text file In_channel.input_all |> Str.(split (regexp "\n"))
;;

let read_paragraph file =
  In_channel.with_open_text file In_channel.input_all |> Str.(split (regexp "\n\n"))
;;

let print_listof_ints ?(txt = "") ints =
  Format.printf
    "%s %a \n"
    txt
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt x -> Format.fprintf fmt "%Ld" x))
    ints
;;

let print_listof_int_tuple ?(txt = "") ints =
  Format.printf
    "%s %a \n"
    txt
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt (x, y) -> Format.fprintf fmt "(%d, %d)" x y))
    ints
;;

let print_listof_strs ?(txt = "") strs =
  Format.printf
    "%s %a \n"
    txt
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       Format.pp_print_string)
    strs
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
