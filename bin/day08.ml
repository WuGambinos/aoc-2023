open Core

let _final_destination = "ZZZ"

type map_info = string * (string * string)

let _print_map_info (info : map_info) =
  let dst, instr = info in
  let left, right = instr in
  let output = Printf.sprintf "DST: %s LEFT: %s RIGHT: %s" dst left right in
  print_endline output
;;

let empty : map_info = "", ("", "")

let is_empty (info : map_info) =
  let dst, instr = info in
  let left, right = instr in
  String.equal dst "" && String.equal left "" && String.equal right ""
;;

let parse_left_right_instruction instruction =
  let result = String.split instruction ~on:',' in
  let left = List.hd_exn result in
  let left = String.slice left 1 4 in
  let right = List.last_exn result in
  let right = String.slice right 1 4 in
  left, right
;;

let _follow_instructions = ()

let find_dst_in_map map (og_dst : string) : map_info =
  List.find_exn map ~f:(fun element ->
    let dst, _ = element in
    String.equal og_dst dst)
;;

let parse_map map =
  let result_map =
    List.fold map ~init:[] ~f:(fun acc line ->
      let split = String.split line ~on:'=' in
      let destination = split |> List.hd_exn |> String.strip in
      let left_right_instr = split |> List.last_exn |> String.strip in
      let left, right = parse_left_right_instruction left_right_instr in
      let element : map_info = destination, (left, right) in
      element :: acc)
  in
  List.rev result_map
;;

let go_direction map (info : map_info) (curr_dir : char) (part1 : bool) =
  let curr_dst, instr = info in
  let left, right = instr in
  let next_destination =
    if Char.equal curr_dir 'L'
    then find_dst_in_map map left
    else find_dst_in_map map right
  in
  let last_index = String.length curr_dst - 1 in
  let last_ch = String.get curr_dst last_index in
  if not part1
  then if Char.equal last_ch 'Z' then empty, true else next_destination, false
  else (
    match curr_dst with
    | "ZZZ" -> empty, true
    | _ -> next_destination, false)
;;

let rec outer_iter
  map
  (info : map_info)
  (instr_list : char list)
  (copy_instr_list : char list)
  (past_result : map_info * bool)
  (steps : int)
  (part_1 : bool)
  =
  match instr_list with
  | [] ->
    let _, cond = past_result in
    if cond
    then steps - 1
    else outer_iter map info copy_instr_list copy_instr_list past_result steps part_1
  | hd :: tl ->
    let result = go_direction map info hd part_1 in
    let info, cond = result in
    if cond
    then outer_iter map info [] copy_instr_list result (steps + 1) part_1
    else outer_iter map info tl copy_instr_list result (steps + 1) part_1
;;

let find_starting_nodes map =
  let result =
    List.fold map ~init:[] ~f:(fun acc element ->
      let dst, _ = element in
      let last_ch = String.get dst (String.length dst - 1) in
      let result = if Char.equal last_ch 'A' then element else empty in
      result :: acc)
  in
  List.filter result ~f:(fun list -> not (is_empty list)) |> List.rev
;;

let rec gcd x y = if y = 0 then x else gcd y (x mod y)
let lcm x y = x / gcd x y * y

let () =
  (* PART 1 *)
  let lines = Advent.read_file "inputs/day8/input.txt" in
  let lines = lines |> List.filter ~f:(fun s -> not (String.is_empty s)) in
  let instructions = lines |> List.hd_exn |> String.to_list in
  let map = List.tl_exn lines in
  let custom_map = parse_map map in
  let _info = empty in
  let start = find_dst_in_map custom_map "AAA" in
  let steps =
    outer_iter custom_map start instructions instructions (empty, false) 0 true
  in
  let part1_answer = Printf.sprintf "PART 1 ANSWER: %d" steps in
  print_endline part1_answer;
  print_endline "";

  (*PART 2 *)
  let starting_nodes = find_starting_nodes custom_map in
  let steps_list =
    List.fold starting_nodes ~init:[] ~f:(fun acc node ->
      let steps =
        outer_iter custom_map node instructions instructions (empty, false) 0 false
      in
      steps :: acc)
  in
  let answer =
    List.fold steps_list ~init:1 ~f:(fun acc n ->
      let result = lcm acc n in
      result)
  in
  print_endline "";
  let part2_answer = Printf.sprintf "PART 2 ANSWER: %d" answer in
  print_endline part2_answer
;;

