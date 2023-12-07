open Base
open Stdio

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

let sum lst = List.fold lst ~init:0 ~f:( + )

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
  let cards_won = num_cards_won winning_numbers my_numbers in
  let points = collect_points winning_numbers my_numbers in
  points, cards_won
;;

let () =
  let lines = Advent.read_file "inputs/day4/input.txt" in
  let part1_answer =
    List.fold lines ~init:0 ~f:(fun acc line ->
      let points, _ = parse_line line in
      points + acc)
  in
  let cards_won_each_round : int list =
    List.fold lines ~init:[] ~f:(fun acc line ->
      let _, res = parse_line line in
      res :: acc)
  in
  let part2_answer =
    List.fold cards_won_each_round ~init:[] ~f:(fun acc cards_won ->
      let res = 1 + sum (List.take acc cards_won) in
      res :: acc)
    |> sum
  in
  let first_output = Printf.sprintf "PART 1 ANSWER: %d" part1_answer in
  let second_output = Printf.sprintf "PART 2 ANSWER: %d" part2_answer in
  print_endline first_output;
  print_endline second_output
;;
