module type Graph = sig
  module Node : Map.OrderedType

  type t

  val empty : t

  val add_node : t -> Node.t -> t
  val add_edge : t -> Node.t -> Node.t -> t

  val nodes : t -> Node.t list
  val neighbors : t -> Node.t -> Node.t list
end

module MakeGraph (Node : Map.OrderedType) : Graph with module Node = Node =
struct
  module Node = Node
  module NodeMap = Map.Make (Node)

  (** map nodes to their adjacency lists *)
  type t = Node.t list NodeMap.t

  let empty = NodeMap.empty

  let add_node g node = if NodeMap.mem node g then g else NodeMap.add node [] g
  let add_edge g src dst =
    let g = add_node g src in
    let g = add_node g dst in
    let neighbors = NodeMap.find src g in
    NodeMap.add src (dst :: neighbors) g

  let nodes g = NodeMap.fold (fun k _ acc -> k :: acc) g []

  let neighbors g node = NodeMap.find_opt node g |> Option.value ~default:[]
end

module Make (G : Graph) = struct
  module NodeMap = Map.Make (G.Node)

  type state =
    | Visited  (** fully explored, all descendants processed *)
    | Visiting  (** currently on the stack (part of the current path) *)

  let rec dfs graph state node =
    let process_new () =
      let state = NodeMap.add node Visiting state in
      let neighbors = G.neighbors graph node in
      (* recursively visit all neighbors *)
      match go_neighbors graph state neighbors with
      | None ->
          None (* cycle found in subtree *)
      | Some state ->
          (* all neighbors explored *)
          Some (NodeMap.add node Visited state)
    in
    match NodeMap.find_opt node state with
    | None ->
        (* not yet visited *)
        process_new ()
    | Some Visiting ->
        (* part of current path. found a cycle *)
        None
    | Some Visited ->
        (* fully explored. skip *)
        Some state

  (* go through all neighbors with dfs *)
  and go_neighbors graph state = function
    | [] ->
        Some state
    | node :: tl -> (
      match dfs graph state node with
      | None ->
          None
      | Some state ->
          go_neighbors graph state tl )

  let has_cycle graph =
    let rec f state = function
      | [] ->
          false (* all nodes explored. no cycle *)
      | node :: tl -> (
        match dfs graph state node with
        | None ->
            true (* found a cycle *)
        | Some state ->
            f state tl )
    in
    f NodeMap.empty (G.nodes graph)
end
