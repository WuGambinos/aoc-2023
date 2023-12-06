open Base
open Stdio

let l1 = [ 1; 2; 3; 4; 5 ]
let l2 = [ 1; 2; 3; 4; 5 ]
let test_lst = [ 0; 0; 0; 0; 0 ]

let increase_at_index lst index =
  List.mapi lst ~f:(fun i n -> if index = i then n + 1 else n)
;;

let combine_two_lists l1 l2 ~f:fn = List.map2_exn l1 l2 ~f:fn

let print_list lst =
  List.iter lst ~f:(fun n ->
    let output = Printf.sprintf "NUM: %d" n in
    print_endline output)
;;

let _ =
  let l2 = increase_at_index l2 2 in
  let _lst = combine_two_lists l1 l2 ~f:( + ) in
  let res =
    List.foldi test_lst ~init:0 ~f:(fun _idx acc value ->
      let inner = value + 1 in
      acc + inner)
  in
  let output = Printf.sprintf "RES: %d" res in
  print_endline output;
  print_list test_lst
;;

(*
   let _ =
   let lists = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ] in
   let sum_lists = List.fold lists ~init:[] ~f:(fun acc line -> line :: acc) in
   List.iter sum_lists ~f:(fun lst ->
   print_list lst;
   print_endline "")
   ;;
*)

let rec play_round played wins i =
  match played, wins with
  | [], [] -> []
  | p_hd :: _p_tl, w_hd :: w_tl ->
    let updated_played =
      List.mapi ~f:(fun j x -> if j >= i && j <= i + w_hd then x + p_hd else x) played
    in
    (*
       printf "I: %d PLAYED: %s\n" i (String.concat ~sep:" " (List.map string_of_int updated_played));
    *)
    play_round updated_played w_tl (i + 1)
  | _ -> failwith "Invalid input"
;;

let () =
  let lines = 6 in
  let wins = [ 4; 2; 2; 1; 0; 0 ] in
  let played = List.init lines ~f:(fun _ -> 0) in
  let final_played = play_round played wins 0 in
  let sum = List.fold final_played ~init:0 ~f:( + ) in
  let output = Printf.sprintf "SUM: %d" sum in
  print_endline output
;;
