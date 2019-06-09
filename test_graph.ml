module OrdChar = struct
  type t = char
  let compare = Char.compare
  let to_string t = Printf.sprintf "%C" t
end

module CharGraph = Graph.Make(OrdChar)

let main =
  let open CharGraph in
  let g = empty in
  let msg = "After creating empty graph:\nnum_edges should be zero: " in
  let msg = msg ^ (num_edges g |> string_of_int) ^ "\n" in
  let msg = msg ^ "num_vertices should be zero: " in
  let msg = msg ^ (num_vertices g |> string_of_int) ^ "\n" in
  print_string msg;

  print_newline();
  
  print_endline "After add_edge 'a' 'b':";
  let g = g |> add_edge 'a' 'b' in
  print_endline ("num_edges should be 1: " ^ string_of_int (num_edges g));
  print_endline ("num_vertices should be 2: " ^ string_of_int (num_vertices g));

  print_newline ();

  print_endline "Test from_list by building graph from the list [('a', ['b';'d']); ('b', ['c']); ('c', ['a'])]:";
  let list = [('a', ['b';'d']); ('b', ['c']); ('c', ['a'])] in
  let g = from_list list in
  print_endline ("num_edges should be 4: " ^ string_of_int (num_edges g));
  print_endline ("num_vertices should be 4: " ^ string_of_int (num_vertices g));

  print_newline();

  print_endline "Test to_list by printing the list made from the previously made graph: ";
  let rec print_list l =
    let rec print_sublist sl =
      match sl with
      | [] -> ()
      | hd :: tl -> Printf.printf "%C, " hd; print_sublist tl
    in
    match l with
    | (v, vset) :: tl ->
      Printf.printf "%C: " v;
      print_sublist vset;
      print_newline ();
      print_list tl
    | [] -> ()
  in
  print_list (to_list g);

  print_newline ();
  print_endline "Test vertex_degree_list on the graph: ";
  print_endline "Output should be:\n'a': 3\n'b': 2\n'c': 2\n'd': 1";
  let list = vertex_degree_list g in
  let rec print_c_list l =
    match l with
    | (key, card) :: tl -> Printf.printf "%C: %d\n" key card; print_c_list tl
    | [] -> ()
  in
  print_endline "Output: ";
  print_c_list list;

  print_newline ();
  
  print_endline "Test to_string: ";
  print_endline "Output should be:\n'a': 'b', 'd'\n'b': 'c'\n'c': 'a'\n'd':";
  print_endline "Output: ";
  print_string (to_string g);
