
type graph = G of (char list * (char * char) list)

let create () = G ([], [])

let add_vertex vertex graph =
  match graph with
  | G (v, e) -> G ((vertex :: v), e)

let add_edge (src, dst) graph =
  match graph with
  | G (v, e) -> G (v, (src, dst) :: e)

let num_vertices graph =
  match graph with
  | G ([], _) -> 0
  | G (v, e) -> List.length v

let num_edges graph =
  match graph with
  | G (_, []) -> 0
  | G (v, (e)) -> List.length e

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



(*
e = [(a,b); (b,c); (c,a); (b,d); (d,a)]
Algoritm för (aux e sv ev):
- Titta på den första kanten i e
- Spara kantens element (a, b) som sv = b, ev = a
- Anropa aux e (sv = b) (ev = a)
*)
