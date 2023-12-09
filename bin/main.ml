open Core

let _ = print_endline "Hello World"
(*
   let sort_slice start_pos end_pos lst =
  let slice = List.slice lst start_pos end_pos in
  let sorted_slice = List.sort ~compare slice in
  let prefix = List.take lst start_pos in
  let suffix = List.drop lst end_pos in
  prefix @ sorted_slice @ suffix
;;

let () =
  let lst = [ 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 ] in
  let start_pos = 2 in
  let end_pos = 8 in
  let sorted_list = sort_slice start_pos end_pos lst in
  printf "Original list: %s\n" (List.to_string ~f:Int.to_string lst);
  printf "Sorted slice: %s\n" (List.to_string ~f:Int.to_string sorted_list)
;;
*)
