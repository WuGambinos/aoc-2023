open Base

let read_file file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;