module type OrderedType = sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
end

module type Graph = sig
  type key
  type graph
  val empty : graph
  val num_vertices : graph -> int
  val add_vertex : key -> graph -> graph
  (**
     add_edge a b adds the directed edge a -> b to the graph
  *)
  val add_edge : key -> key -> graph -> graph
  val num_edges : graph -> int
  (*  val num_cycles : graph -> int *)
  (* val longest_path : graph -> int *)
  val vertex_degree_list : graph -> (key * int) list
  val from_list : (key * key list) list -> graph
  val to_list : graph -> (key * key list) list
  val to_string : graph -> string
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
    (* Add the dst vertex to the src VertexSet *)
    let srcnewset = VertexSet.add dst srcset in
    AdjMap.add src srcnewset g

  let num_vertices graph = AdjMap.cardinal graph

  let num_edges graph =
      let count_edges key vset acc = VertexSet.cardinal vset + acc
      in
      AdjMap.fold count_edges graph 0
(*
  let cycles graph =
    let rec aux k vset acc =
      let f v a =
        if v = k then v :: acc
        else aux k (AdjMap.find v) (v :: acc)
    in
    VertexSet.fold f vset acc
  in
  AdjMap.fold aux graph []*)

  let to_list graph =
    let rec to_tuple k vset acc =
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

  let to_string graph =
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

