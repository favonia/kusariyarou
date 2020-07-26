open StdLabels
open MoreLabels
open Kusariyarou

module IntGraph = Make (Int)
module IntSet = Set.Make (Int)

let vertices = List.init ~len:30 ~f:Fun.id
let empty_graph = List.fold_left ~f:(fun g v -> IntGraph.add_vertex v g) ~init:IntGraph.empty vertices

let test_reachability =
  QCheck_alcotest.to_alcotest ~long:true ~verbose:true @@
  let vertice_generator = QCheck.(choose @@ List.map ~f:always vertices) in
  QCheck.Test.make ~count:10000 ~name:"all-pair reachability"
    QCheck.(small_list @@ pair vertice_generator vertice_generator) @@ fun edges ->
  let graph = List.fold_left ~f:(fun g (u, v) -> IntGraph.add_edge u v g) ~init:empty_graph edges in
  begin
    Fun.flip Stdlib.List.iter vertices @@ fun u ->
    Fun.flip Stdlib.List.iter vertices @@ fun v ->
    ignore @@ IntGraph.reachable u v graph
  end;
  true

let () =
  Alcotest.run "Kusariyarou" [
    "throughput (10000 cases)", [ test_reachability ]
  ]
