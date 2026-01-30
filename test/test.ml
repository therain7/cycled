open Cycled

module IntGraph = MakeGraph (Int)
module IntCycled = Make (IntGraph)

let of_edges =
  List.fold_left
    (fun g (src, dst) -> IntGraph.add_edge g src dst)
    IntGraph.empty

let%test "empty graph has no cycle" = not (IntCycled.has_cycle IntGraph.empty)

let%test "single node no edges has no cycle" =
  let g = IntGraph.add_node IntGraph.empty 1 in
  not (IntCycled.has_cycle g)

let%test "two nodes no cycle" =
  let g = of_edges [(1, 2)] in
  not (IntCycled.has_cycle g)

let%test "simple self loop is a cycle" =
  let g = of_edges [(1, 1)] in
  IntCycled.has_cycle g

let%test "two node cycle" =
  let g = of_edges [(1, 2); (2, 1)] in
  IntCycled.has_cycle g

let%test "three node cycle" =
  let g = of_edges [(1, 2); (2, 3); (3, 1)] in
  IntCycled.has_cycle g

let%test "linear chain has no cycle" =
  let g = of_edges [(1, 2); (2, 3); (3, 4); (4, 5)] in
  not (IntCycled.has_cycle g)

let%test "diamond shape no cycle" =
  (*     1
        / \
       2   3
        \ /
         4    *)
  let g = of_edges [(1, 2); (1, 3); (2, 4); (3, 4)] in
  not (IntCycled.has_cycle g)
