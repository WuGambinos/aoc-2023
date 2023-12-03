let () = print_endline "Hello, World!"
let read_file (file : string) = In_channel.with_open_bin file In_channel.input_all

let list_from_file_str (file : string) =
  List.filter
    (fun x -> x |> String.length > 0)
    (String.split_on_char '\n' (read_file file))
;;

(* FOLD EXAMPLE *)
(*
   let () =
   let nums = [ 1; 2; 3; 4; 5 ] in
   let res = List.fold_left (fun x y -> x + y) 0 nums in
   Printf.printf "SUM: %d\n\n" res
   ;;
*)

let is_digit ch = Char.code ch >= 48 && Char.code ch <= 57

(* DAY 1 PART 1 ORIGINAL METHOD (NAIVE) *)
(*
   let lines = list_from_file_str "input.txt"

   let rec num_list_from_jumbled_str str i lst fn =
   if i == String.length str then
   if List.length lst == 1 then
   let s = String.make 2 (List.hd lst) in
   int_of_string s
   else
   let len = List.length lst in
   let s2 = String.make 1 (List.nth lst 0) in
   let s1 = String.make 1 (List.nth lst (len - 1)) in
   int_of_string (s1 ^ s2)
   else
   let ch = String.get str i in
   if fn ch == true then
   let new_lst = List.cons (String.get str i) lst in
   num_list_from_jumbled_str str (i + 1) new_lst fn
   else num_list_from_jumbled_str str (i + 1) lst fn

   let rec custom_iter lst acc =
   match lst with
   | [] -> acc
   | hd :: tl ->
   let num = num_list_from_jumbled_str hd 0 [] is_digit in
   custom_iter tl (acc + num)

   let res = custom_iter lines 0
   let _ = Printf.printf "DAY 1 PART 1 Answer: %d\n\n" res
*)

(** DAY 1  PART 1 BETTER METHOD **)

let () =
  let lines = list_from_file_str "day1/input.txt" in
  let res =
    List.fold_left
      (fun total line ->
        let chars = String.to_seq line in
        let chars = List.of_seq chars in
        let numbers = List.filter is_digit chars in
        let number =
          Printf.sprintf "%c%c" (List.hd numbers) (List.hd (List.rev numbers))
        in
        let number = int_of_string number in
        total + number)
      0
      lines
  in
  Printf.printf "DAY 1 PART 1 ANSWER: %d\n\n" res
;;

(** Day 2 **)

(*
   let () =
   let lines = list_from_file_str "test2.txt" in
   let res =
   List.fold_left
   (fun total line ->
   let chars = String.to_seq line in
   let chars = List.of_seq chars in
   let numbers = List.filter is_digit chars in
   let number =
   Printf.sprintf "%c%c" (List.hd numbers) (List.hd (List.rev numbers))
   in
   let number = int_of_string number in
   total + number )
   0 lines
   in
   Printf.printf "DAY 1 PART 2 ANSWER: %d" res
*)

(*
   let cases =
   [
    ("one", "1");
    ("two", "2");
    ("three", "3");
    ("four", "4");
    ("five", "5");
    ("six", "6");
    ("seven", "7");
    ("eight", "8");
    ("nine", "9");
    ("1", "1");
    ("2", "2");
    ("3", "3");
    ("4", "4");
    ("5", "5");
    ("6", "6");
    ("7", "7");
    ("8", "8");
    ("9", "9");
  ]

   let map_to_number pos str =
   List.find_map cases ~f:(fun (substr, value) ->
   match String.substr_index ~pos str ~pattern:substr with
   | Some 0 -> Some value
   | _ -> None)

   let str_to_numbers str =
   List.range 0 (String.length str)
   |> List.filter_map ~f:(fun pos -> map_to_number pos str)
*)
