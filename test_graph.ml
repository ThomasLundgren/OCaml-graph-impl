module OrdChar = struct
  type t = char
  let compare = Char.compare
end

module CharGraph = Graph.Make(OrdChar)

let main =
  let open CharGraph in
  let g = empty in
  let msg = "After creating empty graph:\n\tnum_edges should be zero: " in
  let msg = msg ^ (num_edges g |> string_of_int) ^ "\n" in
  let msg = msg ^ "\tnum_vertices should be zero: " in
  let msg = msg ^ (num_vertices g |> string_of_int) ^ "\n" in
  print_string msg;

  print_newline();
  
  print_endline "After adding an edge 'a' -> 'b':";
  let g = g |> add_edge 'a' 'b' in
  print_endline ("\tnum_edges should be 1: " ^ string_of_int (num_edges g));
  print_endline ("\tnum_vertices should be 2: " ^ string_of_int (num_vertices g));

  print_newline ();

  print_endline "Test from_list by building graph from the list [('a', ['b';'d']); ('b', ['c']); ('c', ['a'])]:";
  let list = [('a', ['b';'d']); ('b', ['c']); ('c', ['a'])] in
  let g = from_list list in
  print_endline ("\tnum_edges should be 4: " ^ string_of_int (num_edges g));
  print_endline ("\tnum_vertices should be 4: " ^ string_of_int (num_vertices g));

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
  print_list (to_list g)
