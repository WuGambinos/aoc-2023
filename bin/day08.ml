open Core

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

let find_dst_in_map map og_dst =
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
      print_endline left_right_instr;
      let element = destination, (left, right) in
      element :: acc)
  in
  List.rev result_map
;;

let () =
  let lines = Advent.read_file "inputs/day8/test.txt" in
  let lines = lines |> List.filter ~f:(fun s -> not (String.is_empty s)) in
  let _instructions = List.hd_exn lines in
  let map = List.tl_exn lines in
  List.iter map ~f:print_endline;
  let custom_map = parse_map map in
  List.iter custom_map ~f:(fun element ->
    let dst, instr = element in
    let left, right = instr in
    let dst_fd, _ = find_dst_in_map custom_map right in
    let output =
      Printf.sprintf "DST: %s LEFT: %s RIGHT: %s FD: %s" dst left right dst_fd
    in
    print_endline output)
;;
