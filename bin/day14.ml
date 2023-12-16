open Core

let _ =
  let lines = Advent.read_file "inputs/day14/test.txt" in
  List.iter lines ~f:print_endline
;;
