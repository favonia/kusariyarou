(** This library is based on "Amortized Efficiency of a Path Retrieval Data Structure" by G.F. Italiano, probably with O(log n) slowdown. *)

module type S =
sig
  (** The type of directed graphs. *)
  type t

  (** The type of the vertices. *)
  type vertex

  (** The empty graph. *)
  val empty : t

  (** [mem_vertex v g] tests whether the vertex [v] is in a graph [g]. *)
  val mem_vertex : vertex -> t -> bool

  (** [add_vertex v g] adds a vertex [v] into the graph [g]. *)
  val add_vertex : vertex -> t -> t

  (** [add_edge u v g] adds an edge from [u] to [v] into the graph [g]. The two vertices must be already in the graph, or [Not_found] will be raised. *)
  val add_edge : vertex -> vertex -> t -> t

  (** [reachable u v g] tests whether there is a path from [u] to [v] in the transitive closure of [g]. (This means reachability is always reflexive.) If either vertex is not in the graph, [Not_found] will be raised. *)
  val reachable : vertex -> vertex -> t -> bool
end

module Make : functor (V : Map.OrderedType) -> S with type vertex = V.t
