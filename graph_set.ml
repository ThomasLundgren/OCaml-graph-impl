module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module type GRAPH = sig
  type key
  type 'a graph
  val empty: key graph
  val add_vertex: key -> 'a graph -> 'a graph
  val add_edge: (key * key) -> 'a graph -> 'a graph
  val num_vertices: 'a graph -> int
  val num_edges: 'a graph -> int 
end

module Make (Ord: OrderedType) : GRAPH with type key = Ord.t = struct

  module VertexSet = Set.Make(Ord: OrderedType)
  module OrderedPair = struct
    type t = (Ord.t * Ord.t)
    let compare (x0, y0) (x1, y1) =
      match Ord.compare x0 x1 with
      | 0 -> Ord.compare y0 y1
      | c -> c
  end
  module EdgeSet = Set.Make(OrderedPair)

  type key = Ord.t
  
  type 'a graph =
    | Empty
    | G of {v: VertexSet.t; e: EdgeSet.t}

  let empty = Empty
  
  exception EmptyGraph of string
  
  let add_vertex vertex graph =
    match graph with
    | Empty -> G {v = VertexSet.empty; e = EdgeSet.empty}
    | G {v; e} -> G {v = VertexSet.add vertex v; e}

  let add_edge (src, dst) graph =
    match graph with
    | Empty -> raise (EmptyGraph "Empty graph argument in fun add_edge")
    | G {v; e} -> G {v; e = EdgeSet.add (src, dst) e}

  let num_vertices graph =
    match graph with
    | Empty -> 0
    | G {v} -> VertexSet.cardinal v

  let num_edges graph =
    match graph with
    | Empty -> 0
    | G {e} -> EdgeSet.cardinal e


(*
  let count_cycles graph =
    match graph with
    | G (v, e) ->
      let rec aux edges sv ev =
        match edges with
        | [] -> 0
        | hd :: tl ->
          match hd with
          | (a, c) when a = sv -> aux tl c ev  
          | (a, c) when c = ev -> 1
          | (a, c) -> 0
      in
      let a = List.hd e in
      let b = fst a in
      aux e b b




e = [(a,b); (b,c); (c,a); (b,d); (d,a)]
Algoritm för (aux e sv ev):
- Titta på den första kanten i e
- Spara kantens element (a, b) som sv = b, ev = a
- Anropa aux e (sv = b) (ev = a)
*)
end

