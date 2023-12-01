open Core

let lines = Advent.read_lines "./inputs/day01.in"

let () =
  lines
  |> List.fold_left ~init:0 ~f:(fun acc line ->
    let chars = line |> String.to_list |> List.filter ~f:Char.is_digit in
    if List.length chars > 0
    then (
      let number =
        String.of_char_list [ List.hd_exn chars; List.last_exn chars ]
      in
      acc + Int.of_string number)
    else acc)
  |> Fmt.pr "%d\n"
;;

let num_to_digit num =
  let matched = Str.matched_string num in
  match matched with
  | "one" -> "1"
  | "two" -> "2"
  | "three" -> "3"
  | "four" -> "4"
  | "five" -> "5"
  | "six" -> "6"
  | "seven" -> "7"
  | "eight" -> "8"
  | "nine" -> "9"
  | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> matched
  | _ -> failwith "lol"
;;

let () =
  let pattern =
    Str.regexp {|one\|two\|three\|four\|five\|six\|seven\|eight\|nine|}
  in
  lines
  |> List.fold_left ~init:0 ~f:(fun acc line ->
    let line = Str.global_substitute pattern num_to_digit line in
    let chars =
      line
      |> String.to_list
      |> List.filter ~f:(fun c -> Char.(c >= '1' && c <= '9'))
    in
    if List.length chars > 0
    then
      acc
      + Int.of_string
          (String.of_char_list [ List.hd_exn chars; List.last_exn chars ])
    else acc)
  |> Fmt.pr "%d\n"
;;
