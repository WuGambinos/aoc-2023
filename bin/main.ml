open Base
open Stdio

let rec iter lst =
  match lst with
  | [] -> ()
  | hd :: tl ->
    let _ = print_endline hd in
    iter tl
;;

let () =
  let lines : string list = Advent.read_file "day3/test.txt" in
  (*
     let _ = List.iter lines ~f:print_endline in
  *)
  let _ = iter lines in
  ()
;;
