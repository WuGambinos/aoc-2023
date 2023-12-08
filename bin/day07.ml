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

let int_of_hand hand =
  match hand with
  | HighCard -> 1
  | OnePair -> 2
  | TwoPair -> 3
  | ThreeOfAKind -> 4
  | FullHouse -> 5
  | FourOfAKind -> 6
  | FiveOfAKind -> 7
;;

let string_of_hand hand =
  match hand with
  | HighCard -> "High Card"
  | OnePair -> "One Pair"
  | TwoPair -> "Two Pair"
  | ThreeOfAKind -> "Three Of A Kind"
  | FullHouse -> "Full House"
  | FourOfAKind -> "Four of a kind"
  | FiveOfAKind -> "Five of a kind"
;;

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
*)

let _compare_hand_char a b =
  let a = int_of_hand_char a in
  let b = int_of_hand_char b in
  Int.compare a b
;;

let rec _compare_two_strings a b i =
  let f = int_of_hand_char (String.get a i) in
  let s = int_of_hand_char (String.get b i) in
  if Int.compare f s = 0 then _compare_two_strings a b (i + 1) else Int.compare f s
;;

let _compare_hand_str a b =
  let a = String.to_list a in
  let b = String.to_list b in
  let res =
    List.fold2_exn a b ~init:0 ~f:(fun acc a b ->
      let res = if Char.equal a b then 0 else Char.compare a b in
      acc + res)
  in
  res
;;

let calc_hand hand table =
  List.iter hand ~f:(fun c -> increment_val_of_key table c);
  (*
     let _ =
     Hashtbl.iter_keys table ~f:(fun k ->
     let count = Hashtbl.find_exn table k in
     let output = Printf.sprintf "KEY: %c VALUE: %d" k count in
     print_endline output)
     in
  *)
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
      then _compare_hand_char a_ch b_ch
      else 0
    in
    res + acc)
;;

let custom_compare (first_hand : hand) (second_hand : hand) : int =
  let res = Poly.compare first_hand second_hand in
  let a = int_of_hand first_hand in
  let b = int_of_hand second_hand in
  Int.compare a b |> Int.to_string |> print_endline;
  res |> Int.to_string |> print_endline;
  let a_str = string_of_hand first_hand in
  let b_str = string_of_hand second_hand in
  print_endline a_str;
  print_endline b_str;
  print_endline "";
  if a = b
  then (
    let res = if String.equal a_str b_str then 0 else compare_string a_str b_str in
    res)
  else if a > b
  then 1
  else -1
;;

let sort_by_hand_strength lst =
  List.sort lst ~compare:(fun (_, first_hand, _) (_, second_hand, _) ->
    (*
       let a1 = int_of_hand b_hand in
       let x1 = int_of_hand y_hand in
       Int.compare a1 x1
    *)
    custom_compare first_hand second_hand)
;;

let () =
  let lines = Advent.read_file "inputs/day7/test.txt" in
  let table = create_table in
  let lst =
    List.fold lines ~init:[] ~f:(fun acc line ->
      let split : string list = line |> String.split ~on:' ' in
      let value : int = split |> List.last_exn |> Int.of_string in
      let hand_str = split |> List.hd_exn in
      let hand : char list = hand_str |> String.to_list in
      let hand : char list = List.rev (List.sort hand ~compare:Char.compare) in
      (*
         print_endline "";
      *)
      let table = reset_table table in
      let hand = calc_hand hand table in
      (*
         print_endline line;
         print_endline hand_str;
      *)
      (hand_str, hand, value) :: acc)
  in
  let lst = sort_by_hand_strength lst in
  List.iter lst ~f:(fun (hand_str, hand, value) ->
    let hand = hand |> string_of_hand in
    let output = Printf.sprintf "HAND_STR: %s HAND: %s value: %d" hand_str hand value in
    print_endline output);
  (*
     let answer =
     List.foldi lst ~init:0 ~f:(fun idx acc (_, _, value) ->
     let res = (idx + 1) * value in
     acc + res)
     in
     answer |> Int.to_string |> print_endline;
  *)
  (*
     let lst =
     List.sort lst ~compare:(fun (a, _b, _c) (x, _y, _z) -> compare_two_strings a x 0)
     in
     List.iter lst ~f:(fun (hand_str, hand, value) ->
     let output = Printf.sprintf "HAND_STR: %s HAND: %s value: %d" hand_str hand value in
     print_endline output;
     print_endline "");
  *)
  ()
;;
