open Core

type galaxy =
  { x : int
  ; y : int
  ; ident : int
  }

let print_galaxy galaxy = Printf.printf "IDENT: %d " galaxy.ident

let _print_pair (pair : galaxy * galaxy) =
  let first, second = pair in
  print_galaxy first;
  print_galaxy second;
  print_endline ""
;;

let equal_galaxy (first : galaxy) (second : galaxy) = first.ident = second.ident

let equal_pair (first : galaxy * galaxy) (second : galaxy * galaxy) =
  let first_l, first_r = first in
  let second_l, second_r = second in
  (equal_galaxy first_l second_l && equal_galaxy first_r second_r)
  || (equal_galaxy first_l second_r && equal_galaxy first_r second_r)
;;

let compare_pair (first : galaxy * galaxy) (second : galaxy * galaxy) =
  let first_l, first_r = first in
  let second_l, second_r = second in
  let res = Int.compare first_l.ident second_l.ident in
  if res = 0 then Int.compare first_r.ident second_r.ident else res
;;

let rec inner_collect_galaxy_pairs
  (acc : (galaxy * galaxy) list)
  (g : galaxy)
  (galaxies : galaxy list)
  : (galaxy * galaxy) list
  =
  match galaxies with
  | [] -> acc
  | hd :: tl ->
    let pair =
      if g.ident > hd.ident then g, hd else if g.ident < hd.ident then hd, g else g, hd
    in
    let new_acc = pair :: acc in
    inner_collect_galaxy_pairs new_acc g tl
;;

let outer_collect_galaxy_pairs (galaxies : galaxy list) =
  List.fold galaxies ~init:[] ~f:(fun acc galaxy ->
    let result = inner_collect_galaxy_pairs [] galaxy galaxies in
    result :: acc)
  |> List.concat
;;

let is_empty (lines : char list) =
  let result =
    List.fold lines ~init:true ~f:(fun acc ch ->
      let result = if Char.equal ch '.' then true else false in
      result && acc)
  in
  result
;;

let rec repeat_cons value n acc =
  if n = 0 then acc else repeat_cons value (n - 1) (value :: acc)
;;

let add_extra_space (n : int) (lines : char list list) =
  List.fold lines ~init:[] ~f:(fun acc line ->
    if is_empty line
    then (
      let result = repeat_cons line n acc in
      result)
    else line :: acc)
;;

let manhattan_distance (first : galaxy) (second : galaxy) =
  Int.abs (first.x - second.x) + Int.abs (first.y - second.y)
;;

let replace_hashes_with_numbers matrix =
  let counter = ref 0 in
  let replace_row row =
    List.map
      ~f:(fun cell ->
        if Char.equal cell '#'
        then (
          counter := !counter + 1;
          !counter)
        else 0)
      row
  in
  List.map ~f:replace_row matrix
;;

let filter_none (galaxies_map : galaxy option list list) : galaxy option list list =
  let res =
    List.fold galaxies_map ~init:[] ~f:(fun acc line ->
      let result = List.filter line ~f:Option.is_some in
      result :: acc)
  in
  res
;;

let extract_some (galaxies_map : galaxy option list list) : galaxy list list =
  let res =
    List.fold galaxies_map ~init:[] ~f:(fun acc line ->
      let result = List.map line ~f:(fun v -> Option.value_exn v) in
      result :: acc)
  in
  res
;;

let sum_of_distances file n =
  let lines = Advent.read_file file in
  let lines =
    List.fold lines ~init:[] ~f:(fun acc line ->
      let result = String.to_list line in
      result :: acc)
  in
  let lines =
    lines
    |> add_extra_space n
    |> List.transpose_exn
    |> add_extra_space n
    |> List.transpose_exn
  in
  let lines =
    List.fold lines ~init:[] ~f:(fun acc line ->
      let result = List.rev line in
      result :: acc)
    |> List.rev
  in
  let lines = lines |> replace_hashes_with_numbers in
  let lines =
    List.foldi lines ~init:[] ~f:(fun i acc row ->
      let result =
        List.mapi row ~f:(fun j n ->
          if n = 0
          then None
          else (
            let g : galaxy = { x = i; y = j; ident = n } in
            Some g))
      in
      result :: acc)
    |> List.rev
  in
  let galaxies = lines |> filter_none |> extract_some |> List.concat in
  let pairs =
    galaxies
    |> outer_collect_galaxy_pairs
    |> List.sort ~compare:compare_pair
    |> List.remove_consecutive_duplicates ~equal:equal_pair
    |> List.filter ~f:(fun pair ->
      let left, right = pair in
      not (left.ident = right.ident))
  in
  let answer =
    List.fold pairs ~init:0 ~f:(fun acc pair ->
      let first, second = pair in
      let result = manhattan_distance first second in
      result + acc)
  in
  answer
;;

let part_2 file n =
  let x1 = 10 in
  let x2 = 100 in
  let y1 = sum_of_distances file x1 in
  let y2 = sum_of_distances file x2 in
  let m = (y2 - y1) / (x2 - x1) in
  let eq x = (m * (x - x1)) + y1 in
  eq n
;;

let _ =
  let part1_answer = sum_of_distances "inputs/day11/input.txt" 2 in
  Printf.printf "PART 1 ANSWER: %d\n" part1_answer;
  let part2_answer = part_2 "inputs/day11/input.txt" 1_000_000 in
  Printf.printf "PART 2 ANSWER %d\n" part2_answer
;;
