let line_stream_of_channel channel =
  Stream.from
    (fun _ ->
       try Some (input_line channel) with End_of_file -> None)

let process_line line =
  print_endline line
(*
let process_lines lines  =
  Stream.iter process_line lines
*)
let read_file filename =
  let in_chan = open_in filename in
  try
    Stream.iter process_line (line_stream_of_channel in_chan);
    close_in in_chan
  with e ->
    close_in in_chan;
    raise e

let _ = read_file "test_read.txt"
