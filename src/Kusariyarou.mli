(** This library is based on "Amortized Efficiency of a Path Retrieval Data Structure" by G.F. Italiano *)

module type S =
sig
  type t
  type vertex
  val empty : t
  val mem : vertex -> t -> bool
  val add_vertex : vertex -> t -> t
  val reachable : vertex -> vertex -> t -> bool
  val add_edge : vertex -> vertex -> t -> t
end

module Make : functor (V : Map.OrderedType) -> S with type vertex = V.t
