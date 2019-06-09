module type OrderedType = sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
end

module type Graph = sig

  (**
     The type of the elements stored in the graph.
  *)
  type key

  (**
     The type of the graph data structure.
  *)
  type graph

  (**
     Constructs and empty graph.
  *)
  val empty : graph

  (**
     Returns the number of vertices in the graph.
  *)
  val num_vertices : graph -> int

  (**
     Adds a vertex to the graph.
  *)
  val add_vertex : key -> graph -> graph

  (**
     add_edge a b adds the directed edge a -> b to the graph
  *)
  val add_edge : key -> key -> graph -> graph

  (**
     Returns the number of edges in the graph.
  *)
  val num_edges : graph -> int
  (*  val num_cycles : graph -> int *)
  (* val longest_path : graph -> int *)

  (**
     Returns a list of tuples containing the key and its degree.
  *)
  val vertex_degree_list : graph -> (key * int) list

  (**
     Constructs a graph from a list on the form
     [(k1, [a1; ...; an]); ...; (kn, [a'1; ...; a'n])]
     where k is a key (vertex) in the graph and a and a' are the
     vertices that k connects to.
  *)
  val from_list : (key * key list) list -> graph

  (**
     Constructs from a graph a list on the form
     [(k1, [a1; ...; an]); ...; (kn, [a'1; ...; a'n])]
     where k is a key (vertex) in the graph and a and a' are the
     vertices that k connects to.
  *)
  val to_list : graph -> (key * key list) list

  (**
     Returns a string representation of the graph.
  *)
  val string_of_graph : graph -> string

  (**
     SKA DENNA VARA SYNLIG???
  *)
  val paths : graph -> key -> key -> key list list

  (**
     SKA DENNA VARA SYNLIG???
  *)
  val cycles : graph -> key -> key list list

end

(* Implementation of a directed graph. *)
module Make (Ord: OrderedType) : Graph with type key = Ord.t = struct

  (* A set containing the vertices of a graph *)
  module VertexSet = Set.Make(Ord)
  (* An adjacency map that maps keys of type Ord.t to a set of
     vertices *)
  module AdjMap = Map.Make(Ord)

  (* 
     Type information:
     Graph.key = Ord.t = AdjMap.key = VertexSet.elt
     AdjMap.key = Ord.t
     AdjMap.'a t = 'a Map.Make(Ord).t - the type of the map
     VertexSet.elt = Ord.t
     VertexSet.t = Set.Make(Ord).t - the type of sets
  *)

  type key = Ord.t

  type graph = VertexSet.t AdjMap.t

  let empty = AdjMap.empty

  let add_vertex vertex graph =
    match (AdjMap.mem vertex graph) with
    | true -> graph
    | false -> AdjMap.add vertex VertexSet.empty graph

  let add_edge (src : key) (dst : key) graph =
    let g =
      (* If a key for dst exists, g = graph *)
      if AdjMap.mem dst graph then graph
      (* If a key for dst does not exist, add the key dst to graph *)
      else AdjMap.add dst VertexSet.empty graph in
    let srcset =
      (* Get the VertexSet of the key src *)
      try AdjMap.find src g
      (* If the key src doesn't exist, return an empty VertexSet *)
      with Not_found -> VertexSet.empty
    in
    let srcnewset = VertexSet.add dst srcset in
    AdjMap.add src srcnewset g

  let num_vertices graph = AdjMap.cardinal graph

  let num_edges graph =
    let count_edges key vset acc = VertexSet.cardinal vset + acc
    in
    AdjMap.fold count_edges graph 0

  let edge_list graph =
    let get_edges  key set acc =
      let list = VertexSet.elements set in
      let rec edges l a =
        match l with
        | [] -> a
        | hd :: tl -> edges tl ((key, hd) :: a)
      in
      edges list acc
    in
    AdjMap.fold get_edges graph []

  let neighbors edges v cond =
    let edge l (a, b) = if b = v && cond a then a :: l
      else l in
    List.fold_left edge [] edges

  let rec list_path edges a to_b =
    match to_b with
    | [] -> assert false
    | a' :: _ ->
      if a' = a then [to_b]
      else
        let n = neighbors edges a' (fun c -> not(List.mem c to_b)) in
        List.concat (List.map (fun c -> list_path edges a (c :: to_b)) n)

  let paths graph a b =
    let edges = edge_list graph in
    assert (a <> b);
    list_path edges a [b]

  let cycles graph a =
    let edges = edge_list graph in
    let n = neighbors edges a (fun _ -> true) in
    let p = List.concat (List.map (fun c -> list_path edges a [c]) n) in
    List.map (fun p -> p @ [a]) p

  let to_list graph =
    let to_tuple k vset acc =
      (k, VertexSet.elements vset) :: acc
    in
    AdjMap.fold to_tuple graph [] |> List.rev

  let from_list list =
    let rec readlist list map =
      match list with
      | [] -> map
      | (key, vlist) :: tl ->
        let f graph v =
          add_edge key v graph
        in
        let newmap =
          List.fold_left f map vlist
        in
        readlist tl newmap
    in
    readlist list empty

  let vertex_degree_list graph =
    let degree k vset acc =
      let in_degree k' vset' acc' =
        if k' = k then acc' + 1
        else acc'
      in
      let out_degree = VertexSet.cardinal vset in
      let full_degree = AdjMap.fold in_degree graph 0 + out_degree
      in
      (k, full_degree) :: acc
    in
    AdjMap.fold degree graph [] |> List.rev

  let string_of_graph graph =
    let list = to_list graph in
    let rec list_to_string l acc =
      let rec subl_to_string subl acc =
        match subl with
        | [] -> acc ^ "\n"
        | hd :: [] -> let acc' = acc ^ (Ord.to_string hd) in subl_to_string [] acc'
        | hd :: tl -> let acc' = acc ^ (Ord.to_string hd ^ ", ") in subl_to_string tl acc'
      in
      match l with
      | [] -> acc
      | (key, vlist) :: tl ->
        let acc' = acc ^ (Ord.to_string key ^ ": " ^ subl_to_string vlist "")
        in
        list_to_string tl acc'
    in list_to_string list ""

end

