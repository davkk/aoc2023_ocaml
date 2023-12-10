type tile =
  | Ground
  | Start
  | NS
  | WE
  | NE
  | NW
  | SW
  | SE
[@@deriving show]

type direction =
  | Up
  | Down
  | Left
  | Right
  | Stop

let parse_lines lines =
  let start_x = ref 0 in
  let start_y = ref 0 in
  let pipes =
    lines
    |> List.mapi (fun y line ->
      String.to_seq line
      |> Seq.mapi (fun x c ->
        match c with
        | '.' -> Ground
        | 'S' ->
          start_x := x;
          start_y := y;
          Start
        | '|' -> NS
        | '-' -> WE
        | 'L' -> NE
        | 'J' -> NW
        | '7' -> SW
        | 'F' -> SE
        | _ -> failwith "Invalid pipe")
      |> Array.of_seq)
    |> Array.of_list
  in
  let start = !start_x, !start_y in
  pipes, start
;;

let check_bounds pipes (x, y) =
  let x_max = Array.length pipes in
  let y_max = Array.length pipes.(0) in
  if x >= 0 && x < x_max && y >= 0 && y < y_max then true else false
;;

let direction_to_coords = function
  | Up -> 0, -1
  | Down -> 0, 1
  | Left -> -1, 0
  | Right -> 1, 0
  | Stop -> 0, 0
;;

let next_move source curr_pipe =
  begin
    match source, curr_pipe with
    | Down, NS -> Down
    | Up, NS -> Up
    | Right, WE -> Right
    | Left, WE -> Left
    | Down, NE -> Right
    | Left, NE -> Up
    | Down, NW -> Left
    | Right, NW -> Up
    | Up, SW -> Left
    | Right, SW -> Down
    | Up, SE -> Right
    | Left, SE -> Down
    | Up, Start | Down, Start | Right, Start | Left, Start -> Stop
    | _ -> failwith "shouldn't be here"
  end
;;

module Part1 = struct
  let walk_loop start tiles =
    let x_max = Array.length tiles in
    let y_max = Array.length tiles.(0) in
    let rec aux prev (x, y) acc =
      let curr_tile = tiles.(y).(x) in
      let next_direction = next_move prev curr_tile in
      match next_direction with
      | Stop -> acc + 1
      | _ -> begin
        let dx, dy = direction_to_coords next_direction in
        aux next_direction (x + dx, y + dy) (acc + 1)
      end
    in
    let x, y = start in
    let directions =
      [ Up; Right; Down; Left ]
      |> List.map (fun dir ->
        let dx, dy = direction_to_coords dir in
        if x + dx >= 0 && x + dx < x_max && y + dy >= 0 && y + dy < y_max
        then Some tiles.(y + dy).(x + dx)
        else None)
    in
    let initial_move =
      match directions with
      | [ Some NS; _; _; _ ] | [ Some SE; _; _; _ ] | [ Some SW; _; _; _ ] -> Up
      | [ _; Some WE; _; _ ] | [ _; Some NW; _; _ ] | [ _; Some SW; _; _ ] ->
        Right
      | [ _; _; Some NS; _ ] | [ _; _; Some NE; _ ] | [ _; _; Some NW; _ ] ->
        Down
      | [ _; _; _; Some WE ] | [ _; _; _; Some NE ] | [ _; _; _; Some SE ] ->
        Left
      | _ -> failwith "cannot start move"
    in
    let dx, dy = direction_to_coords initial_move in
    aux initial_move (x + dx, y + dy) 0
  ;;
end

let () =
  let lines = Advent.read_lines "./inputs/day10.in" in
  let pipes, start = parse_lines lines in
  let result = Part1.walk_loop start pipes / 2 in
  Fmt.pr "@.%d@." result
;;

(* module Part2 = struct *)
(*   let walk_loop start tiles = *)
(*     let x_max = Array.length tiles in *)
(*     let y_max = Array.length tiles.(0) in *)
(*     let rec aux prev (x, y) acc = *)
(*       let curr_tile = tiles.(y).(x) in *)
(*       let next_direction = next_move prev curr_tile in *)
(*       match next_direction with *)
(*       | Stop -> acc *)
(*       | _ -> begin *)
(*         let dx, dy = direction_to_coords next_direction in *)
(*         aux next_direction (x + dx, y + dy) ((x, y) :: acc) *)
(*       end *)
(*     in *)
(*     let x, y = start in *)
(*     let directions = *)
(*       [ Up; Right; Down; Left ] *)
(*       |> List.map (fun dir -> *)
(*         let dx, dy = direction_to_coords dir in *)
(*         if x + dx >= 0 && x + dx < x_max && y + dy >= 0 && y + dy < y_max *)
(*         then Some tiles.(y + dy).(x + dx) *)
(*         else None) *)
(*     in *)
(*     let initial_move = *)
(*       match directions with *)
(*       | [ Some NS; _; _; _ ] | [ Some SE; _; _; _ ] | [ Some SW; _; _; _ ] -> Up *)
(*       | [ _; Some WE; _; _ ] | [ _; Some NW; _; _ ] | [ _; Some SW; _; _ ] -> *)
(*         Right *)
(*       | [ _; _; Some NS; _ ] | [ _; _; Some NE; _ ] | [ _; _; Some NW; _ ] -> *)
(*         Down *)
(*       | [ _; _; _; Some WE ] | [ _; _; _; Some NE ] | [ _; _; _; Some SE ] -> *)
(*         Left *)
(*       | _ -> failwith "cannot start move" *)
(*     in *)
(*     let dx, dy = direction_to_coords initial_move in *)
(*     aux initial_move (x + dx, y + dy) [] *)
(*   ;; *)
(* end *)
(**)
(* module IntMap = Map.Make (Int) *)
(**)
(* let () = *)
(*   let lines = Advent.read_lines "./inputs/day10.in" in *)
(*   let tiles, start = parse_lines lines in *)
(*   let result = *)
(*     Part2.walk_loop start tiles *)
(*     |> List.fold_left *)
(*          (fun acc (x, y) -> *)
(*            IntMap.update *)
(*              y *)
(*              (function *)
(*                | Some value -> Some (x :: value) *)
(*                | None -> Some (x :: [])) *)
(*              acc) *)
(*          IntMap.empty *)
(*     |> IntMap.to_seq *)
(*     |> Seq.map (fun (_, row) -> *)
(*       Fmt.pr "@.%a" (Fmt.list ~sep:(fun _ () -> Fmt.pr ", ") Fmt.int) row; *)
(*       row *)
(*       |> List.fold_left *)
(*            (fun (acc, add_next) step -> *)
(*              if add_next then acc - step, true else acc, false) *)
(*            (List.hd row, false)) *)
(*     |> Seq.fold_left (fun acc row_total -> acc + fst row_total) 0 *)
(*   in *)
(*   Fmt.pr "@.%d@." result *)
(* ;; *)
