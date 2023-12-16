open Core

let print_char_list lst =
  List.iter lst ~f:(fun c -> Printf.printf "%c" c);
  print_endline ""
;;

let _find_reflection _lines = ()

let _ =
  let lines = Advent.read_file "inputs/day13/test.txt" in
  let lines = List.map lines ~f:(fun line -> String.to_list line) in
  let lines = List.take lines 2 in
  (*
     let head = List.hd_exn lines in
     print_char_list head
  *)
  List.iter lines ~f:print_char_list
;;
