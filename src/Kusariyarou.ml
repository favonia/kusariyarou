open StdLabels
open MoreLabels

module type S =
sig
  type t
  type vertex
  val empty : t
  val mem_vertex : vertex -> t -> bool
  val add_vertex : vertex -> t -> t
  val add_edge : vertex -> vertex -> t -> t
  val reachable : vertex -> vertex -> t -> bool
end

module Make (V : Map.OrderedType) : S with type vertex = V.t =
struct
  type vertex = V.t

  module VMap = Map.Make (V)
  type node = vertex list
  type spanning = node VMap.t
  type t = spanning VMap.t

  let empty : t = VMap.empty

  let mem_vertex (v : vertex) (g : t) : bool = VMap.mem v g

  let add_vertex (v : vertex) (g : t) : t =
    VMap.add ~key:v ~data:(VMap.singleton v []) g

  let reachable (u : vertex) (v : vertex) (g : t) =
    if not (mem_vertex u g && mem_vertex v g) then raise Not_found;
    VMap.mem v (VMap.find u g)

  let add_edge (u : vertex) (v : vertex) (g : t) =
    if not (mem_vertex u g && mem_vertex v g) then raise Not_found;
    if reachable u v g then g else
      let mv = VMap.find v g in
      let add_pointer i j m =
        VMap.update ~key:i ~f:(fun l -> Some (j :: Option.get l)) @@
        VMap.add ~key:j ~data:[] m
      in
      let rec meld i mx =
        let f mx j = if VMap.mem j mx then mx else meld j @@ add_pointer i j mx in
        List.fold_left ~f ~init:mx (VMap.find i mv)
      in
      let f mx =
        if VMap.mem u mx && not (VMap.mem v mx) then
          meld v @@ add_pointer u v mx
        else
          mx
      in
      VMap.map ~f g
end
