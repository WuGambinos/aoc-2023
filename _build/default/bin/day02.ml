open Base
open Stdio

let red = 12
let green = 13
let blue = 14

(* DAY 2 PART 1 *)
(*
   let parse_round round =
   let round = String.strip round in
   let info = round |> String.split ~on:' ' in
   let num = List.hd_exn info in
   let color = info |> List.rev |> List.hd_exn in
   let num = Int.of_string num in
   (num, color)

   let rec parse_rounds (rounds : string list) (valid : bool) =
   match rounds with
   | [] -> valid
   | hd :: tl ->
   let score, color = parse_round hd in
   let res =
   match color with
   | "red" -> score <= red
   | "blue" -> score <= blue
   | "green" -> score <= green
   | _ -> false
   in
   parse_rounds tl (valid && res)

   let inner_iter (lst : string list) = parse_rounds lst true

   let parse_game game =
   let first_part : string list = game |> List.hd_exn |> String.split ~on:':' in
   let rest : string list = game |> List.tl_exn in

   let _game_number : string = List.hd_exn first_part in
   let first : string = List.nth_exn first_part 1 in
   let rounds : string list = first |> String.split ~on:',' in
   let round_result : bool = parse_rounds rounds true in

   let rec parse_rest (lst : string list) valid =
   match lst with
   | [] -> valid
   | hd :: tl ->
   let split : string list = hd |> String.split ~on:',' in
   let res : bool = inner_iter split in
   parse_rest tl (valid && res)
   in

   let rest = parse_rest rest true in
   let result = round_result && rest in
   result

   let _ =
   let lines : string list = read_file "day2/input.txt" in
   let answer =
   List.foldi lines ~init:0 ~f:(fun idx acc line ->
   let game_parts = String.split line ~on:';' in
   let res = if parse_game game_parts then idx + 1 else 0 in
   acc + res)
   in
   let output = Printf.sprintf "DAY 2 PART 1 ANSWER: %d" answer in
   print_endline output
*)

(* DAY 2 PART 2  MIGHT TRY TO CLEAN UP *)
let parse_round round =
  let round = String.strip round in
  let info = round |> String.split ~on:' ' in
  let num = List.hd_exn info in
  let color = info |> List.rev |> List.hd_exn in
  let num = Int.of_string num in
  num, color
;;

let rec parse_rounds
  (rounds : string list)
  (valid : bool)
  (max_red : int)
  (max_green : int)
  (max_blue : int)
  =
  match rounds with
  | [] -> valid, max_red, max_green, max_blue
  | hd :: tl ->
    let score, color = parse_round hd in
    let valid, max_red, max_green, max_blue =
      match color with
      | "red" ->
        let res = score <= red in
        res, Int.max score max_red, max_green, max_blue
      | "green" ->
        let res = score <= green in
        res, max_red, Int.max score max_green, max_blue
      | "blue" ->
        let res = score <= blue in
        res, max_red, max_green, Int.max score max_blue
      | _ -> false, 0, 0, 0
    in
    parse_rounds tl (valid && false) max_red max_green max_blue
;;

let rec parse_rest (lst : string list) valid max_red max_green max_blue =
  match lst with
  | [] -> valid, max_red, max_green, max_blue
  | hd :: tl ->
    let split : string list = hd |> String.split ~on:',' in
    let result, new_max_red, new_max_green, new_max_blue =
      parse_rounds split true 0 0 0
    in
    let red = Int.max max_red new_max_red in
    let green = Int.max max_green new_max_green in
    let blue = Int.max max_blue new_max_blue in
    parse_rest tl (valid && result) red green blue
;;

let parse_game game =
  let first_part : string list = game |> List.hd_exn |> String.split ~on:':' in
  let rest : string list = game |> List.tl_exn in
  let _game_number : string = List.hd_exn first_part in
  let first : string = List.nth_exn first_part 1 in
  let rounds : string list = first |> String.split ~on:',' in
  let _valid, first_red, first_green, first_blue = parse_rounds rounds true 0 0 0 in
  let _valid, max_red, max_green, max_blue = parse_rest rest true 0 0 0 in
  let final_red = Int.max first_red max_red in
  let final_green = Int.max first_green max_green in
  let final_blue = Int.max first_blue max_blue in
  final_red * final_green * final_blue
;;

let _ =
  let lines : string list = Advent.read_file "day2/input.txt" in
  let answer =
    List.fold lines ~init:0 ~f:(fun acc line ->
      let game_parts = String.split line ~on:';' in
      let result = parse_game game_parts in
      acc + result)
  in
  let output = Printf.sprintf "DAY 2 PART 2 ANSWER: %d" answer in
  print_endline output
;;
