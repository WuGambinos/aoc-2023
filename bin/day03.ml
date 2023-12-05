open Base
open Stdio

type info =
  { left : int
  ; right : int
  ; number : int
  }
[@@deriving show]

let string_of_info info =
  Printf.sprintf "LEFT: %d RIGHT: %d NUMBER: %d" info.left info.right info.number
;;

let _ =
  let test : info = { left = 0; right = 0; number = 5 } in
  test |> string_of_info |> print_endline
;;

let ( let* ) res f = Option.bind res ~f
let not_digit_or_period ch = not (Char.is_digit ch || Char.equal ch '.')

(* ALGORITHM

   1. Iterate over list of strings
   2. Turn each string into char list
   3. Start parsing numbers, (store in "info" record)
   4. Combine list of "info" records into one list
   5. Iterate over list of infos
   6. Iterate from left integer to right integer and check neighbors of cell for any symbol
*)

let cell (parts_list : char list list) (r : int) (c : int) : char option =
  let* (row : char list) = List.nth parts_list r in
  let* (cell : char) = List.nth row c in
  Some cell
;;

let check_neighbors parts_list r c =
  let left = cell parts_list r (c - 1) in
  let right = cell parts_list r (c + 1) in
  let up = cell parts_list (r - 1) c in
  let down = cell parts_list (r + 1) c in
  let result_left =
    match left with
    | Some ch -> if not_digit_or_period ch then true else false
    | None -> false
  in
  let result_right =
    match right with
    | Some ch -> if not_digit_or_period ch then true else false
    | None -> false
  in
  let result_up =
    match up with
    | Some ch -> if not_digit_or_period ch then true else false
    | None -> false
  in
  let result_down =
    match down with
    | Some ch -> if not_digit_or_period ch then true else false
    | None -> false
  in
  result_left || result_right || result_down || result_up
;;

let () =
  let lines : string list = Advent.read_file "inputs/day3/test.txt" in
  let lines : char list list = List.map lines ~f:(fun s -> String.to_list s) in
  let _ =
    List.iteri lines ~f:(fun r line ->
      List.iteri line ~f:(fun c _ch ->
        let cell = cell lines r c in
        let cell = Option.value_exn cell in
        if Char.is_digit cell
        then
          if check_neighbors lines r c
          then (
            let result = Printf.sprintf "ROW: %d COL: %d CH: %c\n" r c cell in
            print_endline result)))
  in
  ()
;;
