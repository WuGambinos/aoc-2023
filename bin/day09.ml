open Core
open Stdio

let _print_list (list : int list) =
  List.iter list ~f:(fun n ->
    let output = Printf.sprintf "%d " n in
    print_string output);
  print_endline ""
;;

let diff_between_pairs pair =
  let x, y = pair in
  y - x
;;

let all_zero list =
  List.fold list ~init:true ~f:(fun acc n ->
    let result = if n = 0 then true else false in
    result && acc)
;;

let collect_differences pairs =
  List.fold pairs ~init:[] ~f:(fun acc pair ->
    let result = diff_between_pairs pair in
    result :: acc)
;;

let rec pairs_of_list lst =
  match lst with
  | [] | [ _ ] -> []
  | x :: y :: rest -> (x, y) :: pairs_of_list (y :: rest)
;;

let rec custom_fold (acc : int list list) (list : int list) : int list list =
  if all_zero list
  then acc
  else (
    let pairs = pairs_of_list list in
    let differences : int list = pairs |> collect_differences |> List.rev in
    let result = List.append acc [ differences ] in
    custom_fold result differences)
;;

let change_last_item (lst : int list) (new_value : int) =
  match List.rev lst with
  | [] -> []
  | _ :: rev_rest -> List.rev (new_value :: rev_rest)
;;

let add_values (curr_row : int list) (next_row : int list) =
  let x = List.last_exn curr_row in
  let y = List.nth_exn next_row (List.length next_row - 2) in
  x + y
;;

let subtract_values (curr_row : int list) (next_row : int list) =
  let x = List.last_exn curr_row in
  let y = List.nth_exn next_row (List.length next_row - 2) in
  y - x
;;

let rec extrapolate
  (lst : int list list)
  (acc : int list list)
  (next_value : int)
  (i : int)
  ~part_1
  =
  match lst with
  | [] -> acc
  | hd :: tl ->
    let new_list = change_last_item hd next_value in
    let cond = List.hd tl in
    let next_value =
      match cond with
      | Some tl -> if part_1 then add_values new_list tl else subtract_values new_list tl
      | None -> 0
    in
    let new_acc = new_list :: acc in
    extrapolate tl new_acc next_value (i + 1) ~part_1
;;

let () =
  let lines = Advent.read_file "inputs/day9/input.txt" in
  let part1_answer =
    List.fold lines ~init:0 ~f:(fun acc line ->
      let nums = line |> String.split ~on:' ' |> List.map ~f:Int.of_string in
      let result_list = custom_fold [ nums ] nums in
      let result_list =
        List.fold result_list ~init:[] ~f:(fun inner_acc list ->
          let result = List.append list [ 0 ] in
          result :: inner_acc)
      in
      let final_list = extrapolate result_list [] 0 0 ~part_1:true in
      let extrapolated_val = List.last_exn (List.hd_exn final_list) in
      extrapolated_val + acc)
  in
  let part2_answer =
    List.fold lines ~init:0 ~f:(fun acc line ->
      let nums = line |> String.split ~on:' ' |> List.map ~f:Int.of_string in
      let result_list = custom_fold [ nums ] nums in
      let result_list =
        List.fold result_list ~init:[] ~f:(fun inner_acc list ->
          let result = List.rev_append list [ 0 ] in
          result :: inner_acc)
      in
      let final_list = extrapolate result_list [] 0 0 ~part_1:false in
      let extrapolated_val = List.last_exn (List.hd_exn final_list) in
      extrapolated_val + acc)
  in
  let p1_answer = Printf.sprintf "PART 1 ANSWER: %d" part1_answer in
  let p2_answer = Printf.sprintf "PART 2 ANSWER: %d" part2_answer in
  print_endline p1_answer;
  print_endline p2_answer
;;
