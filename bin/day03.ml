let lines = Advent.read_lines "./inputs/day03.in"

type token =
  | Number of string * int * int
  | Symbol of string * int

(* let string_of_engine e =
   match e with
   | Number (n, start, end_) -> Printf.sprintf "Number(%s, %d, %d)" n start end_
   | Symbol (s, idx) -> Printf.sprintf "Symbol(%s, %d)" s idx
   ;; *)

let parse_symbols =
  let pattern = Re.Perl.compile_pat {|(\d+)|([^.])|} in
  List.fold_left
    (fun acc line ->
      let matches = Re.all pattern line in
      let symbols =
        matches
        |> List.filter_map (fun m ->
          let number = Re.Group.get_opt m 1 in
          let symbol = Re.Group.get_opt m 2 in
          match number, symbol with
          | Some n, None ->
            let start, end_ = Re.Group.offset m 1 in
            Some (Number (n, start, end_ - 1))
          | None, Some s ->
            let idx, _ = Re.Group.offset m 2 in
            Some (Symbol (s, idx))
          | _ -> None)
        |> Array.of_list
      in
      [| symbols |] |> Array.append acc)
    [||]
;;

let sum_adjacent idx line =
  line
  |> Array.fold_left
       (fun acc symbol ->
         acc
         +
         match symbol with
         | Number (n, start, end_) when idx >= start - 1 && idx <= end_ + 1 ->
           int_of_string n
         | _ -> 0)
       0
;;

let () =
  let symbols = lines |> parse_symbols in
  let max_symbols = Array.length symbols in
  let result =
    symbols
    |> Base.Array.foldi ~init:0 ~f:(fun line_idx total_acc line ->
      let adjacent =
        line
        |> Base.Array.fold ~init:0 ~f:(fun acc symbol ->
          match symbol with
          | Symbol (_, s_idx) -> begin
            let prev =
              if line_idx - 1 >= 0
              then sum_adjacent s_idx symbols.(line_idx - 1)
              else 0
            in
            let curr = sum_adjacent s_idx line in
            let next =
              if line_idx + 1 < max_symbols
              then sum_adjacent s_idx symbols.(line_idx + 1)
              else 0
            in
            acc + prev + curr + next
          end
          | _ -> acc)
      in
      total_acc + adjacent)
  in
  Fmt.pr "@.%d@." result
;;

let find_adjacent idx line =
  line
  |> Base.Array.filter_map ~f:(fun symbol ->
    match symbol with
    | Number (n, start, end_) when idx >= start - 1 && idx <= end_ + 1 ->
      Some (int_of_string n)
    | _ -> None)
;;

let () =
  let symbols = lines |> parse_symbols in
  let max_symbols = Array.length symbols in
  let result =
    symbols
    |> Base.Array.foldi ~init:0 ~f:(fun line_idx total_acc line ->
      let adjacent =
        line
        |> Base.Array.fold ~init:0 ~f:(fun acc symbol ->
          match symbol with
          | Symbol (s, s_idx) when s = "*" -> begin
            let prev =
              if line_idx - 1 >= 0
              then find_adjacent s_idx symbols.(line_idx - 1)
              else [||]
            in
            let curr = find_adjacent s_idx line in
            let next =
              if line_idx + 1 < max_symbols
              then find_adjacent s_idx symbols.(line_idx + 1)
              else [||]
            in
            let combined = Array.concat [ prev; curr; next ] in
            if Array.length combined = 2
            then acc + Base.Array.fold ~init:1 ~f:( * ) combined
            else acc
          end
          | _ -> acc)
      in
      total_acc + adjacent)
  in
  Fmt.pr "%d@." result
;;
