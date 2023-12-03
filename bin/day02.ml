open Base
open Stdio

let read_file file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

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
    (match color with
     | "red" ->
       let res = score <= red in
       if score > max_red
       then parse_rounds tl (valid && res) score max_green max_blue
       else parse_rounds tl (valid && res) max_red max_green max_blue
     | "green" ->
       let res = score <= green in
       if score > max_green
       then parse_rounds tl (valid && res) max_red score max_blue
       else parse_rounds tl (valid && res) max_red max_green max_blue
     | "blue" ->
       let res = score <= blue in
       if score > max_blue
       then parse_rounds tl (valid && res) max_red max_green score
       else parse_rounds tl (valid && res) max_red max_green max_blue
     | _ -> parse_rounds tl (valid && false) max_red max_green max_blue)
;;

let inner_iter (lst : string list) = parse_rounds lst true 0 0 0

let parse_game game =
  let first_part : string list = game |> List.hd_exn |> String.split ~on:':' in
  let rest : string list = game |> List.tl_exn in
  let _game_number : string = List.hd_exn first_part in
  let first : string = List.nth_exn first_part 1 in
  let rounds : string list = first |> String.split ~on:',' in
  let round_result, first_red, first_green, first_blue = parse_rounds rounds true 0 0 0 in
  let rec parse_rest (lst : string list) valid max_red max_green max_blue =
    match lst with
    | [] -> valid, max_red, max_green, max_blue
    | hd :: tl ->
      let split : string list = hd |> String.split ~on:',' in
      let result, new_max_red, new_max_green, new_max_blue = inner_iter split in
      let red = Int.max max_red new_max_red in
      let green = Int.max max_green new_max_green in
      let blue = Int.max max_blue new_max_blue in
      parse_rest tl (valid && result) red green blue
  in
  let rest, max_red, max_green, max_blue = parse_rest rest true 0 0 0 in
  let _result = round_result && rest in
  let final_red = Int.max first_red max_red in
  let final_green = Int.max first_green max_green in
  let final_blue = Int.max first_blue max_blue in
  final_red * final_green * final_blue
;;

let _ =
  let lines : string list = read_file "day2/input.txt" in
  let answer =
    List.foldi lines ~init:0 ~f:(fun _idx acc line ->
      let game_parts = String.split line ~on:';' in
      let result = parse_game game_parts in
      acc + result)
  in
  let output = Printf.sprintf "DAY 2 PART 2 ANSWER: %d" answer in
  print_endline output
;;

