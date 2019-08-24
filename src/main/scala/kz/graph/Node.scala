package kz.graph

sealed trait Node {
  type T
  def value: T
  def children: Set[Node]
  def name: String
}

final case class GenericNode[G](override val value: G,
                                override val children: Set[Node],
                                override val name: String) extends Node {
  override type T = G
}