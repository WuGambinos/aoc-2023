open Core

let final_destination = "ZZZ"

type map_info = string * (string * string)

let print_map_info (info : map_info) =
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
  (*
     let output = Printf.sprintf "LEFT: %s RIGHT: %s" left right in
     print_endline output;
  *)
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
      (*
         print_endline left_right_instr;
      *)
      let element : map_info = destination, (left, right) in
      element :: acc)
  in
  List.rev result_map
;;

let go_direction map (info : map_info) (curr_dir : char) =
  let curr_dst, instr = info in
  let left, right = instr in
  let next_destination =
    if Char.equal curr_dir 'L'
    then find_dst_in_map map left
    else find_dst_in_map map right
  in
  let empty : map_info = "", ("", "") in
  let out = "NEXT DESTINATION" in
  print_endline "";
  print_endline out;
  print_map_info next_destination;
  if String.equal curr_dst final_destination then empty, true else next_destination, false
;;

let rec _outer_iter
  map
  (info : map_info)
  (instr_list : char list)
  (copy_instr_list : char list)
  (past_result : map_info * bool)
  (steps : int)
  =
  match instr_list with
  | [] ->
    let _, cond = past_result in
    if cond
    then steps - 1
    else _outer_iter map info copy_instr_list copy_instr_list past_result steps
  | hd :: tl ->
    (*
       print_map_info info;
    *)
    let result = go_direction map info hd in
    let info, cond = result in
    (*
       print_map_info info;
    *)
    steps |> Int.to_string |> print_endline;
    if cond
    then _outer_iter map info [] copy_instr_list result (steps + 1)
    else _outer_iter map info tl copy_instr_list result (steps + 1)
;;

let find_starting_nodes map =
  let result =
    List.fold map ~init:[] ~f:(fun acc element ->
      let dst, _ = element in
      let last_ch = String.get dst (String.length dst - 1) in
      let result = if Char.equal last_ch 'A' then element else empty in
      result :: acc)
  in
  List.filter result ~f:(fun list -> not (is_empty list))
;;

let _all_reached_end nodes =
  List.fold nodes ~init:true ~f:(fun acc item ->
    let final_destination, _ = item in
    let last_char = String.get final_destination (String.length final_destination - 1) in
    let result = if Char.equal last_char 'Z' then true else false in
    result && acc)
;;

let simulatenous_search starting_nodes map =
  let next_nodes =
    List.fold starting_nodes ~f:(fun acc start_node ->
      let result = go_direction map start_node 'L' in
      result :: acc)
  in
  print_endline "NEXT NODES: " in

  ()
;;

let () =
  let lines = Advent.read_file "inputs/day8/test3.txt" in
  let lines = lines |> List.filter ~f:(fun s -> not (String.is_empty s)) in
  let _instructions = lines |> List.hd_exn |> String.to_list in
  let map = List.tl_exn lines in
  let custom_map = parse_map map in
  (*
     List.iter custom_map ~f:(fun element ->
     let dst, instr = element in
     let left, right = instr in
     let dst_fd, _ = find_dst_in_map custom_map right in
     let output =
     Printf.sprintf "DST: %s LEFT: %s RIGHT: %s FD: %s" dst left right dst_fd
     in
     print_endline output)
  *)
  (*
     let _info = empty in
     let start = find_dst_in_map custom_map "AAA" in
     let steps = outer_iter custom_map start instructions instructions (empty, false) 0 in
     steps |> Int.to_string |> print_endline
  *)
  let starting_nodes = find_starting_nodes custom_map in
  (*
     List.iter starting_nodes ~f:print_map_info
  *)
  let _ = simulatenous_search starting_nodes custom_map in
  ()
;;
