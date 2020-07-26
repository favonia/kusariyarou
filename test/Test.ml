open StdLabels
open MoreLabels
open Kusariyarou

module IntGraph = Make (Int)
module IntSet = Set.Make (Int)

let dumb_reachable start target edges =
  let rec go target edges visited : bool =
    IntSet.mem target visited || begin
      let new_visited =
        let f visited (u, v) =
          if IntSet.mem u visited && not (IntSet.mem v visited) then
            IntSet.add v visited
          else
            visited
        in
        List.fold_left ~f ~init:visited edges
      in
      if visited == new_visited then false
      else go target edges new_visited
    end
  in
  go target edges (IntSet.singleton start)

let vertices = List.init ~len:15 ~f:Fun.id
let empty_graph = List.fold_left ~f:(fun g v -> IntGraph.add_vertex v g) ~init:IntGraph.empty vertices

let test_reachability =
  QCheck_alcotest.to_alcotest @@
  let vertice_generator = QCheck.(choose @@ List.map ~f:always vertices) in
  QCheck.Test.make ~count:10000 ~name:"all-pair reachability"
    QCheck.(small_list @@ pair vertice_generator vertice_generator) @@ fun edges ->
  let graph = List.fold_left ~f:(fun g (u, v) -> IntGraph.add_edge u v g) ~init:empty_graph edges in
  Fun.flip Stdlib.List.for_all vertices @@ fun u ->
  Fun.flip Stdlib.List.for_all vertices @@ fun v ->
  IntGraph.reachable u v graph == dumb_reachable u v edges

let () =
  Alcotest.run "Kusariyarou" [
    "reachability", [ test_reachability ]
  ]
