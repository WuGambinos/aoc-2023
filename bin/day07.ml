open Core

let create_table =
  let table = Hashtbl.create (module Char) in
  let _ = Hashtbl.add table ~key:'A' ~data:0 in
  let _ = Hashtbl.add table ~key:'K' ~data:0 in
  let _ = Hashtbl.add table ~key:'Q' ~data:0 in
  let _ = Hashtbl.add table ~key:'J' ~data:0 in
  let _ = Hashtbl.add table ~key:'T' ~data:0 in
  let _ = Hashtbl.add table ~key:'9' ~data:0 in
  let _ = Hashtbl.add table ~key:'8' ~data:0 in
  let _ = Hashtbl.add table ~key:'7' ~data:0 in
  let _ = Hashtbl.add table ~key:'6' ~data:0 in
  let _ = Hashtbl.add table ~key:'5' ~data:0 in
  let _ = Hashtbl.add table ~key:'4' ~data:0 in
  let _ = Hashtbl.add table ~key:'3' ~data:0 in
  let _ = Hashtbl.add table ~key:'2' ~data:0 in
  table
;;

let reset_table table =
  let _ = Hashtbl.set table ~key:'A' ~data:0 in
  let _ = Hashtbl.set table ~key:'K' ~data:0 in
  let _ = Hashtbl.set table ~key:'Q' ~data:0 in
  let _ = Hashtbl.set table ~key:'J' ~data:0 in
  let _ = Hashtbl.set table ~key:'T' ~data:0 in
  let _ = Hashtbl.set table ~key:'9' ~data:0 in
  let _ = Hashtbl.set table ~key:'8' ~data:0 in
  let _ = Hashtbl.set table ~key:'7' ~data:0 in
  let _ = Hashtbl.set table ~key:'6' ~data:0 in
  let _ = Hashtbl.set table ~key:'5' ~data:0 in
  let _ = Hashtbl.set table ~key:'4' ~data:0 in
  let _ = Hashtbl.set table ~key:'3' ~data:0 in
  let _ = Hashtbl.set table ~key:'2' ~data:0 in
  table
;;

let increment_val_of_key table k =
  let new_value = Hashtbl.find_exn table k + 1 in
  let _ = Hashtbl.set table ~key:k ~data:new_value in
  ()
;;

type hand =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
[@@deriving compare]

let _string_of_hand hand =
  match hand with
  | HighCard -> "High Card"
  | OnePair -> "One Pair"
  | TwoPair -> "Two Pair"
  | ThreeOfAKind -> "Three Of A Kind"
  | FullHouse -> "Full House"
  | FourOfAKind -> "Four of a kind"
  | FiveOfAKind -> "Five of a kind"
;;


(* Day 7 Part 1 *)
(*
   let int_of_hand_char ch =
   match ch with
   | '2' -> 2
   | '3' -> 3
   | '4' -> 4
   | '5' -> 5
   | '6' -> 6
   | '7' -> 7
   | '8' -> 8
   | '9' -> 9
   | 'T' -> 10
   | 'J' -> 11
   | 'Q' -> 12
   | 'K' -> 13
   | 'A' -> 14
   | _ -> -1000
   ;;

   let compare_hand_char a b =
   let a = int_of_hand_char a in
   let b = int_of_hand_char b in
   Int.compare a b
   ;;

   let calc_hand hand table =
   List.iter hand ~f:(fun c -> increment_val_of_key table c);
   let values =
   table
   |> Hashtbl.data
   |> List.sort ~compare:Int.descending
   |> List.filter ~f:(fun n -> not (n = 0))
   in
   let len = List.length values in
   let result =
   if len = 1
   then FiveOfAKind
   else if len = 2
   then (
   match values with
   | [ 4; 1 ] -> FourOfAKind
   | [ 3; 2 ] -> FullHouse
   | _ -> HighCard)
   else if len = 3
   then (
   match values with
   | [ 3; 1; 1 ] -> ThreeOfAKind
   | [ 2; 2; 1 ] -> TwoPair
   | _ -> HighCard)
   else if len = 4
   then OnePair
   else HighCard
   in
   result
   ;;

   let compare_string (first_hand : string) (second_hand : string) =
   let first_hand = String.to_list first_hand in
   let second_hand = String.to_list second_hand in
   List.fold2_exn first_hand second_hand ~init:0 ~f:(fun acc a_ch b_ch ->
   let res =
   if Char.equal a_ch b_ch
   then 0
   else if acc = 0
   then compare_hand_char a_ch b_ch
   else 0
   in
   res + acc)
   ;;

   let custom_compare first_str (first_hand : hand) second_str (second_hand : hand) : int =
   let compare = Poly.compare first_hand second_hand in
   if compare = 0
   then (
   let result =
   if String.equal first_str second_str then 0 else compare_string first_str second_str
   in
   result)
   else compare
   ;;

   let sort_by_hand_strength lst =
   List.sort lst ~compare:(fun (first_str, first_hand, _) (second_str, second_hand, _) ->
   let res = custom_compare first_str first_hand second_str second_hand in
   res)
   ;;

   let () =
   let lines = Advent.read_file "inputs/day7/input.txt" in
   let table = create_table in
   let lst =
   List.fold lines ~init:[] ~f:(fun acc line ->
   let split : string list = line |> String.split ~on:' ' in
   let value : int = split |> List.last_exn |> Int.of_string in
   let hand_str = split |> List.hd_exn in
   let hand : char list = hand_str |> String.to_list in
   let hand : char list = List.rev (List.sort hand ~compare:Char.compare) in
   let table = reset_table table in
   let hand = calc_hand hand table in
   (hand_str, hand, value) :: acc)
   in
   let lst = sort_by_hand_strength lst in
   let answer =
   List.foldi lst ~init:0 ~f:(fun idx acc (_, _, value) ->
   let res = (idx + 1) * value in
   acc + res)
   in
   let part1_out = Printf.sprintf "PART 1 ANSWER: %d" answer in
   print_endline part1_out
   ;;
*)

(* DAY 7 PART 2 *)
let int_of_hand_char ch =
  match ch with
  | 'J' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'T' -> 10
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | _ -> -1000
;;

let compare_hand_char a b =
  let a = int_of_hand_char a in
  let b = int_of_hand_char b in
  Int.compare a b
;;

let update_head list new_head =
  match list with
  | [] -> []
  | _ :: tail -> new_head :: tail
;;

let increase_max_in_list list j =
  let old_head = List.hd_exn list in
  let new_list = update_head list (old_head + j) in
  new_list |> List.filter ~f:(fun n -> not (n = 0))
;;

let calc_hand hand_str hand table =
  List.iter hand ~f:(fun c -> increment_val_of_key table c);
  let j = Hashtbl.find_exn table 'J' in
  let _ = Hashtbl.set table ~key:'J' ~data:0 in
  let values =
    if String.equal hand_str "JJJJJ"
    then table |> Hashtbl.data |> List.sort ~compare:Int.descending
    else
      table
      |> Hashtbl.data
      |> List.sort ~compare:Int.descending
      |> List.filter ~f:(fun n -> not (n = 0))
  in
  let values = increase_max_in_list values j in
  let len = List.length values in
  let result =
    if len = 1
    then FiveOfAKind
    else if len = 2
    then (
      match values with
      | [ 4; 1 ] -> FourOfAKind
      | [ 3; 2 ] -> FullHouse
      | _ -> HighCard)
    else if len = 3
    then (
      match values with
      | [ 3; 1; 1 ] -> ThreeOfAKind
      | [ 2; 2; 1 ] -> TwoPair
      | _ -> HighCard)
    else if len = 4
    then OnePair
    else HighCard
  in
  result
;;

let compare_string (first_hand : string) (second_hand : string) =
  let first_hand = String.to_list first_hand in
  let second_hand = String.to_list second_hand in
  List.fold2_exn first_hand second_hand ~init:0 ~f:(fun acc a_ch b_ch ->
    let res =
      if Char.equal a_ch b_ch
      then 0
      else if acc = 0
      then compare_hand_char a_ch b_ch
      else 0
    in
    res + acc)
;;

let custom_compare first_str (first_hand : hand) second_str (second_hand : hand) : int =
  let compare = Poly.compare first_hand second_hand in
  if compare = 0
  then (
    let result =
      if String.equal first_str second_str then 0 else compare_string first_str second_str
    in
    result)
  else compare
;;

let sort_by_hand_strength lst =
  List.sort lst ~compare:(fun (first_str, first_hand, _) (second_str, second_hand, _) ->
    let res = custom_compare first_str first_hand second_str second_hand in
    res)
;;

let () =
  let lines = Advent.read_file "inputs/day7/input.txt" in
  let table = create_table in
  let lst =
    List.fold lines ~init:[] ~f:(fun acc line ->
      let split : string list = line |> String.split ~on:' ' in
      let value : int = split |> List.last_exn |> Int.of_string in
      let hand_str = split |> List.hd_exn in
      let hand : char list = hand_str |> String.to_list in
      let hand : char list = List.rev (List.sort hand ~compare:Char.compare) in
      let table = reset_table table in
      let hand_type = calc_hand hand_str hand table in
      (hand_str, hand_type, value) :: acc)
  in
  let lst = sort_by_hand_strength lst in
  let answer =
    List.foldi lst ~init:0 ~f:(fun idx acc (_hand_str, _hand_type, value) ->
      let res = (idx + 1) * value in
      acc + res)
  in
  let part2_out = Printf.sprintf "PART 2 ANSWER: %d" answer in
  print_endline part2_out
;;
