open Core

let lines = Advent.read_lines "./inputs/day02.in"

let () =
  let result =
    lines
    |> List.fold ~init:0 ~f:(fun acc line ->
      let line = line |> Str.split (Str.regexp ": ") in
      match line with
      | [ game; all_cubes ] ->
        let game_id =
          String.split ~on:' ' game |> List.last_exn |> Int.of_string
        in
        let is_valid =
          all_cubes
          |> String.split_on_chars ~on:[ ';'; ',' ]
          |> List.for_all ~f:(fun color ->
            let color = String.strip color in
            match String.split ~on:' ' color with
            | [ count; "red" ] when Int.of_string count > 12 -> false
            | [ count; "green" ] when Int.of_string count > 13 -> false
            | [ count; "blue" ] when Int.of_string count > 14 -> false
            | _ -> true)
        in
        if is_valid then acc + game_id else acc
      | _ -> failwith "lol")
  in
  Fmt.pr "@.%d@." result
;;

let () =
  let result =
    lines
    |> List.fold ~init:0 ~f:(fun acc line ->
      let cubes = line |> Str.split (Str.regexp ": ") |> List.last_exn in
      let r, g, b =
        cubes
        |> Str.split (Str.regexp {|; \|, |})
        |> List.fold ~init:(0, 0, 0) ~f:(fun acc color ->
          let r, g, b = acc in
          match String.split ~on:' ' color with
          | [ count; "red" ] when Int.of_string count > r ->
            Int.of_string count, g, b
          | [ count; "green" ] when Int.of_string count > g ->
            r, Int.of_string count, b
          | [ count; "blue" ] when Int.of_string count > b ->
            r, g, Int.of_string count
          | _ -> acc)
      in
      acc + (r * g * b))
  in
  Fmt.pr "@.%d@." result
;;
