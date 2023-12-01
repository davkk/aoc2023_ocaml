open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    In_channel.input_all channel |> String.split_lines)
;;
