open Core
(* HASH ALGORITM

   Get ascii code for curr character in string
   Increase current value by Ascii code
   set current value equal to itself multiplied by 17
   set curren tvalue to remained of divding itself by 256
*)

type lens =
  { op : char
  ; ident : char list
  ; length : int option
  ; hash : int
  }

let _print_lens item =
  let len =
    match item.length with
    | Some n -> n
    | None -> -1
  in
  Printf.printf
    "OP: %c LABEL: %s LENGTH: %d\n"
    item.op
    (List.to_string item.ident ~f:Char.to_string)
    len
;;

let rec hash acc lst =
  match lst with
  | [] -> acc
  | hd :: tl ->
    let ascii = Char.to_int hd in
    let new_acc = acc + ascii in
    let new_acc = new_acc * 17 in
    let new_acc = new_acc mod 256 in
    hash new_acc tl
;;

let same_label left right = List.equal Char.equal left.ident right.ident

let get_lens (lst : char list) =
  let op = if List.mem lst '=' ~equal:Char.equal then '=' else '-' in
  let op_idx, _ = List.findi_exn lst ~f:(fun _ elem -> Char.equal elem op) in
  let len = lst |> List.last_exn |> Char.to_int in
  let len = len - Char.to_int '0' in
  let label = List.take lst op_idx in
  let hsh = hash 0 label in
  if Char.equal op '='
  then { op; ident = label; length = Some len; hash = hsh }
  else { op; ident = label; length = None; hash = hsh }
;;

let rec create_n_entries table n =
  match n with
  | -1 -> ()
  | n ->
    let _ = Hashtbl.add table ~key:n ~data:[] in
    create_n_entries table (n - 1)
;;

let create_table =
  let table = Hashtbl.create (module Int) in
  let _ = create_n_entries table 255 in
  table
;;

let replace_element (lst : lens list) (old_value : lens) (new_value : lens) =
  List.map ~f:(fun x -> if same_label x old_value then new_value else x) lst
;;

let handles_equals (l : lens) table =
  let box = l.hash in
  let current_box : lens list = Hashtbl.find_exn table box in
  let exist = List.mem current_box l ~equal:same_label in
  let updated_box =
    if exist then replace_element current_box l l else List.append current_box [ l ]
  in
  let _ = Hashtbl.set table ~key:box ~data:updated_box in
  ()
;;

let remove_element_from_list lst target =
  List.filter lst ~f:(fun elem -> not (List.equal Char.equal elem.ident target.ident))
;;

let handles_minus (l : lens) table =
  let box = l.hash in
  let current_box = Hashtbl.find_exn table box in
  let updated_box = remove_element_from_list current_box l in
  let _ = Hashtbl.set table ~key:box ~data:updated_box in
  ()
;;

let () =
  let table = create_table in
  let lines = Advent.read_file "inputs/day15/input.txt" in
  let lines = lines |> List.hd_exn |> String.split ~on:',' in
  let lines = lines |> List.map ~f:(fun str -> String.to_list str) in
  let part1_answer =
    List.fold lines ~init:0 ~f:(fun acc str ->
      let hash = hash 0 str in
      hash + acc)
  in
  Printf.printf "PART 1 Answer: %d\n" part1_answer;
  let list_of_lens =
    List.fold lines ~init:[] ~f:(fun acc item ->
      let lens = get_lens item in
      lens :: acc)
    |> List.rev
  in
  let _ =
    List.iter list_of_lens ~f:(fun len ->
      let _ =
        if Char.equal len.op '='
        then handles_equals len table
        else handles_minus len table
      in
      ())
  in
  let part2_answer =
    Hashtbl.fold table ~init:0 ~f:(fun ~key:_key ~data:box acc ->
      let focusing_power =
        if List.is_empty box
        then 0
        else
          List.foldi box ~init:0 ~f:(fun idx inner_acc item ->
            let result = (item.hash + 1) * (idx + 1) * Option.value_exn item.length in
            result + inner_acc)
      in
      focusing_power + acc)
  in
  Printf.printf "PART 2 ANSWER: %d\n" part2_answer
;;
