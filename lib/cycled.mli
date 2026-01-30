module type Graph = sig
  module Node : Map.OrderedType

  type t
  val empty : t

  val add_node : t -> Node.t -> t
  val add_edge : t -> Node.t -> Node.t -> t

  val nodes : t -> Node.t list
  val neighbors : t -> Node.t -> Node.t list
end

module MakeGraph : (Node : Map.OrderedType) -> Graph with module Node = Node

module Make : (G : Graph) -> sig
  val has_cycle : G.t -> bool
end
