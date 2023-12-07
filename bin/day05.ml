open Core

type range_info =
  { from : string
  ; target : string
  ; dst_start : int
  ; src_start : int
  ; length : int
  }

let _print_range_info r =
  let output =
    Printf.sprintf
      "FROM: %s TARGET: %s DST_START: %d SOURCE_START: %d LEN: %d"
      r.from
      r.target
      r.dst_start
      r.src_start
      r.length
  in
  print_endline output;
  ()
;;

let parse_num_list (num_list : string) : int list =
  let nums : string list = String.split num_list ~on:' ' in
  let nums : string list = List.filter nums ~f:(fun s -> not (String.is_empty s)) in
  let nums : int list = List.map nums ~f:Int.of_string in
  nums
;;

let _parse_map_line (map_line : string) : range_info =
  let num_list = parse_num_list map_line in
  let from = "" in
  let target = "" in
  let dst_start, src_start, length =
    List.nth_exn num_list 0, List.nth_exn num_list 1, List.nth_exn num_list 2
  in
  let res : range_info = { from; target; dst_start; src_start; length } in
  res
;;

let () =
  let lines = Advent.read_file "inputs/day5/test.txt" in
  List.iter lines ~f:(fun line -> print_endline line);
  print_endline "";
  let seeds_to_be_planted = lines |> List.hd_exn |> String.split ~on:':' in
  let _seed_nums = parse_num_list (List.last_exn seeds_to_be_planted) in
  (*
     let rest = lines |> List.tl_exn |> List.filter ~f:(fun s -> not (String.is_empty s)) in
  *)
  (*
     List.iter rest ~f:(fun line ->
     if Char.is_alpha (String.get line 0)
     then ()
     else (
     let info = parse_map_line line in
     print_range_info info));
  *)
  (*
     let map_name_list =
     List.fold rest ~init:[] ~f:(fun acc line ->
     let res = if Char.is_alpha (String.get line 0) then line else "" in
     res :: acc)
     |> List.filter ~f:(fun s -> not (String.is_empty s))
     in
     List.iter map_name_list ~f:(fun s -> print_endline s)
  *)
  ()
;;
