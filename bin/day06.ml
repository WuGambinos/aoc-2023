open Core

let distance ~speed:s ~time:t = s * t
let _time ~distance:d ~speed:s = d / s
let _speed ~distance:d ~time:t = d / t

let race push_time end_time max_distance =
  let sp = push_time in
  let time_left = end_time - push_time in
  let dist = distance ~speed:sp ~time:time_left in
  if dist > max_distance then 1 else 0
;;

let () =
  let lines = Advent.read_file "inputs/day6/input.txt" in
  let times =
    lines |> List.hd_exn |> String.split ~on:':' |> List.last_exn |> String.split ~on:' '
  in
  let times =
    times
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:(fun s -> Int.of_string s)
  in
  let distances =
    lines
    |> List.last_exn
    |> String.split ~on:':'
    |> List.last_exn
    |> String.split ~on:' '
  in
  let distances =
    distances
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:(fun s -> Int.of_string s)
  in
  let part1_answer =
    List.fold2_exn times distances ~init:1 ~f:(fun acc time distance ->
      (*
         let s = speed ~distance ~time in
         let output =
         Printf.sprintf "Time: %ds Distance: %dmm Speed: %d mm/s" time distance s
         in
         print_endline output;
      *)
      let list_range = List.range 0 time in
      let different_ways =
        List.fold list_range ~init:0 ~f:(fun acc element ->
          let result = race element time distance in
          acc + result)
      in
      let out = Printf.sprintf "Different Ways This Race: %d" different_ways in
      print_endline out;
      print_endline "";
      acc * different_ways)
  in
  let part1 = Printf.sprintf "PART 1 ANSWER: %d" part1_answer in
  print_endline part1;
  print_endline ""
;;

let () =
  let lines = Advent.read_file "inputs/day6/input.txt" in
  let times =
    lines |> List.hd_exn |> String.split ~on:':' |> List.last_exn |> String.split ~on:' '
  in
  let time =
    times
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.fold ~init:"" ~f:( ^ )
    |> Int.of_string
  in
  let distance =
    lines
    |> List.last_exn
    |> String.split ~on:':'
    |> List.last_exn
    |> String.split ~on:' '
  in
  let distance =
    distance
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.fold ~init:"" ~f:( ^ )
    |> Int.of_string
  in
  let list_range = List.range 0 time in
  let different_ways =
    List.fold list_range ~init:0 ~f:(fun acc element ->
      let result = race element time distance in
      acc + result)
  in
  let part2 = Printf.sprintf "PART 2 ANSWER: %d" different_ways in
  print_endline part2
;;
