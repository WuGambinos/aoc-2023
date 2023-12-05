open Base
open Stdio

(* DAY4 PART1 *)
(*
   let collect_points (l1 : int list) (l2 : int list) =
   let res =
   List.fold l1 ~init:0 ~f:(fun outer_acc outer_num ->
   let answer =
   List.fold l2 ~init:0 ~f:(fun inner_acc inner_num ->
   let win_count = if outer_num = inner_num then 1 else 0 in
   inner_acc + win_count)
   in
   outer_acc + answer)
   in
   match res with
   | 0 -> 0
   | 1 -> 1
   | _ -> Int.pow 2 (res - 1)
   ;;

   let parse_num_list (num_list : string) =
   let nums : string list = String.split num_list ~on:' ' in
   let nums : string list = List.filter nums ~f:(fun s -> not (String.is_empty s)) in
   let nums : int list = List.map nums ~f:Int.of_string in
   nums
   ;;

   let parse_line (line : string) =
   let split_line : string list = String.split line ~on:'|' in
   let split_head : string list = split_line |> List.hd_exn |> String.split ~on:':' in
   let winning_numbers : int list =
   split_head |> List.last_exn |> String.strip |> parse_num_list
   in
   let my_numbers : int list =
   List.nth_exn split_line 1 |> String.strip |> parse_num_list
   in
   collect_points winning_numbers my_numbers
   ;;

   let () =
   let lines = Advent.read_file "inputs/day4/input.txt" in
   let total_points =
   List.fold lines ~init:0 ~f:(fun acc line ->
   let res = parse_line line in
   acc + res)
   in
   let output = Printf.sprintf "TOTAL_POINTS: %d\n" total_points in
   print_endline output
   ;;
*)

(*
   let rec handle_copies lst count times =
   if count = times
   then lst
   else (
   let new_lst = help_handle_copies lst 0 0 times in
   handle_copies new_lst count times)
   ;;
*)

(*
   (* Increase individual card a "max" amount of times *)
   let rec help_handle_copies lst i j max =
   if j >= max
   then lst
   else (
   let new_lst = increase_at_index lst i 1 in
   help_handle_copies new_lst i (j + 1) max)
   ;;
*)
let increase_at_index lst index incr =
  let res =
    List.mapi lst ~f:(fun i n ->
      let output = Printf.sprintf "AFTER: CARD: %d VAL %d" (i + 1) n in
      print_endline output;
      if i = index then n + incr else n)
  in
  print_endline "";
  res
;;

let rec increment_at_index_n_times lst index n =
  if n = 0
  then lst
  else (
    let new_lst = increase_at_index lst index 1 in
    increment_at_index_n_times new_lst index (n - 1))
;;

let help_handle_copies lst start index _cards_won cop =
  if index = start then lst else increment_at_index_n_times lst (index - 1) cop
;;

let rec loop (lst : int list) cards_won st ed cop =
  if st >= ed
  then lst
  else (
    let new_lst = help_handle_copies lst st ed cards_won cop in
    loop new_lst cards_won st (ed - 1) cop)
;;

(*
   let rec count_up lst og_n n cards_won =
   if n >= cards_won + og_n
   then lst
   else (
   let times = List.nth_exn lst (n - 1) in
   let new_lst = increase_at_index lst n times in
   count_up new_lst og_n (n + 1) cards_won)
   ;;
*)

let num_cards_won (l1 : int list) (l2 : int list) =
  let res =
    List.fold l1 ~init:0 ~f:(fun outer_acc outer_num ->
      let answer =
        List.fold l2 ~init:0 ~f:(fun inner_acc inner_num ->
          let win_count = if outer_num = inner_num then 1 else 0 in
          inner_acc + win_count)
      in
      outer_acc + answer)
  in
  res
;;

let parse_num_list (num_list : string) =
  let nums : string list = String.split num_list ~on:' ' in
  let nums : string list = List.filter nums ~f:(fun s -> not (String.is_empty s)) in
  let nums : int list = List.map nums ~f:Int.of_string in
  nums
;;

let parse_line (i : int) (track : int list) (line : string) =
  let split_line : string list = String.split line ~on:'|' in
  let split_head : string list = split_line |> List.hd_exn |> String.split ~on:':' in
  let winning_numbers : int list =
    split_head |> List.last_exn |> String.strip |> parse_num_list
  in
  let my_numbers : int list =
    List.nth_exn split_line 1 |> String.strip |> parse_num_list
  in
  let cards_won = num_cards_won winning_numbers my_numbers in
  (*
     let num_list = count_up track (i + 1) (i + 1) cards_won in
  *)
  let output = Printf.sprintf "START: %d END: %d COP: %d" i (i + cards_won + 1) (i + 1) in
  print_endline output;
  let num_list = loop track cards_won i (i + cards_won + 1) (i + 1) in
  let _ = print_endline "" in
  let output = Printf.sprintf "CARDS WON: %d" cards_won in
  print_endline output;
  List.iter num_list ~f:(fun n -> n |> Int.to_string |> print_endline);
  let sum_of_list = num_list |> List.fold ~init:0 ~f:( + ) in
  sum_of_list
;;

let () =
  let lines = Advent.read_file "inputs/day4/test.txt" in
  let len = List.length lines in
  (*
  let track_count = List.init len ~f:(fun _n -> 1) in
     let total_cards =
     List.foldi lines ~init:0 ~f:(fun idx acc line ->
     let res = parse_line idx track_count line in
     acc + res)
     in
  *)
      let track_count = List.init len ~f:(fun _n -> 1) in
  let total_cards =
    List.foldi lines ~init:0 ~f:(fun idx acc line ->
      let res = parse_line idx track_count line in
      acc + res)
  in
  let output = Printf.sprintf "TOTAL_POINTS: %d\n" total_cards in
  print_endline output
;;
