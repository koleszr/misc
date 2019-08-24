package kz.graph

object GraphApp {

  val node8 = GenericNode(8, Set.empty[Node], "node8")
  val node7 = GenericNode(7, Set(node8), "node7")
  val node6 = GenericNode(6, Set(node8), "node6")
  val node5 = GenericNode(5, Set(node6, node7), "node5")
  val node4 = GenericNode(4, Set(node7), "node4")
  val node3 = GenericNode(3, Set(node6), "node3")
  val node2 = GenericNode(2, Set(node5), "node2")
  val node1 = GenericNode(1, Set(node4, node5), "node1")
  val node0 = GenericNode(0, Set(node1, node2, node3), "node0")

  def main(args: Array[String]): Unit = {
    graphAsMap()
    nodes()
    nodeValues()
    nodeNames()
  }

  def graphAsMap(): Unit = {
    println("Graph represented as a map:")
    Graph.asMap(node0) foreach { case (node, children) => println(s"$node: ${children.mkString("[", ", ", "]")}")}
  }

  def nodes(): Unit = {
    println("Nodes in the graph:")
    println(Graph.nodes(node0).mkString(", "))
  }

  def nodeValues(): Unit = {
    println("Name of nodes in the graph:")
    println(Graph.nodes(node0).map(_.value).mkString(", "))
  }

  def nodeNames(): Unit = {
    println("Value of nodes in the graph:")
    println(Graph.nodes(node0).map(_.name).mkString(", "))
  }
}
