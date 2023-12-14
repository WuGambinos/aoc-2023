open Core 
open Stdio
let () = 
    let lines = Advent.read_file "inputs/day10/test.txt" in
    List.iter lines ~f: print_endline
