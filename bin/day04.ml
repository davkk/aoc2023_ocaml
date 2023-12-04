let lines = Advent.read_lines "./inputs/day04.in"

let process_line line =
  let game = List.nth (String.split_on_char ':' line) 1 |> String.trim in
  let numbers = String.split_on_char '|' game in
  match numbers with
  | [ guess; winning ] ->
    let guess = guess |> String.trim |> String.split_on_char ' ' in
    let winning = winning |> String.trim |> String.split_on_char ' ' in
    guess
    |> List.filter_map (fun g_num ->
      winning
      |> List.find_opt (fun w_num ->
        if w_num = "" || g_num = ""
        then false
        else String.trim g_num = String.trim w_num))
  | _ -> failwith "elephant"
;;

let () =
  let result =
    lines
    |> List.fold_left
         (fun acc line ->
           let overlap = process_line line in
           acc
           + (int_of_float
              @@ Float.pow 2.0
              @@ float_of_int
              @@ (List.length overlap - 1)))
         0
  in
  Fmt.pr "@.%d@." result
;;

let () =
  let total_lines = List.length lines in
  let counts = Base.Array.create ~len:total_lines 0 in
  lines
  |> List.iteri (fun idx line ->
    let overlap = process_line line in
    let cards_won = List.length overlap in
    counts.(idx) <- counts.(idx) + 1;
    if cards_won > 0
    then
      for card_idx = idx + 1 to min (idx + cards_won) (total_lines - 1) do
        counts.(card_idx) <- counts.(card_idx) + counts.(idx)
      done);
  let result = Array.fold_left (fun acc count -> acc + count) 0 counts in
  Fmt.pr "@.%d@." result
;;
