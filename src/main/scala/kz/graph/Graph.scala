package kz.graph

import scala.annotation.tailrec

object Graph {

  def asMap[T](root: Node): Map[String, Set[String]] = {
    @tailrec
    def go(nodes: Set[Node], acc: Map[String, Set[String]] = Map.empty): Map[String, Set[String]] =
      if (nodes.isEmpty) {
        acc
      } else {
        val head = nodes.head
        val children = head.children
        go(children ++ nodes.tail, acc + (head.name -> children.map(_.name)))
      }

    go(Set(root))
  }

  def nodes[T](root: Node): Set[Node] = {
    @tailrec
    def go(visit: Set[Node], visited: Set[Node] = Set.empty): Set[Node] = {
      if (visit.isEmpty) {
        visited
      } else {
        val head = visit.head
        go(head.children ++ visit.tail, visited + head)
      }
    }

    go(Set(root))
  }

}
