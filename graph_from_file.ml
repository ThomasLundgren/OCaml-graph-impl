module OrdChar = struct
  type t = char
  let compare = Char.compare
  let to_string t = Printf.sprintf "%C" t
end

module CharGraph = Graph.Make(OrdChar)

open CharGraph

let line_stream_of_channel channel =
  Stream.from
    (fun _ ->
       try Some (input_line channel) with End_of_file -> None)

let graph_from_line line graph =
  let split_line character line = String.split_on_char character line in
  match split_line '-' line with
  | [] -> graph
  | a :: b :: rest -> (add_edge (String.get a 0) (String.get b 0) graph)
  | a :: [] -> graph

let rec graph_from_stream stream graph =
  match (Stream.peek stream) with
  | None -> graph
  | Some line ->
    let l = Stream.next stream in
    graph_from_stream (stream) (graph_from_line l graph)

let read_file filename =
  let in_chan = open_in filename in
  try
    let g = graph_from_stream (line_stream_of_channel in_chan) empty in
    print_string ("Graph read from file:\n" ^ (string_of_graph g));
    close_in in_chan
  with e ->
    close_in in_chan;
    raise e

let _ = read_file "test_read.txt"
