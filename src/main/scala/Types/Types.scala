package Types
import scala.collection.immutable._
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable._
/**
 * Created by Jacob on 2015-12-07.
 */
object Types {

  type Text = List[Sentence]
  type Sentence = List[Word]
  type IndexedSentence = (Sentence, Int)
  type Word = String

  class Graph[T] {
    class Node[T](v: T){
      var value: T = v
      var edges: List[Edge] = Nil
      override def toString():String = {
        value toString
      }

    }
    class Edge(sN: Node[T], eN: Node[T], w: Double){
      var weight: Double = w
      var startNode: Node[T] = sN
      var endNode: Node[T] = eN
      override def toString: String = {
        "" + startNode.value + "->("+weight+")" + endNode.value
      }
    }
    class Path(sn: Node[T]){
      val startNode = sn
      var path:List[Node[T]] = List(startNode)
      var cost: Double = 0
      def addNode(n: Node[T], c: Double): Unit ={
        path = path ++ List(n)
        cost += c
      }
      override def toString(): String = {
        path mkString "->"
      }
    }
    var nodes: List[Node[T]] = Nil
    var edges: List[Edge] = Nil

    def addNode(value: T): Node[T] ={
      val node = new Node[T](value)
      if (nodes.find(node.equals).isEmpty) {
        nodes = node :: nodes
      }
      node
    }

    def addEdge(start: T, end:T, w: Double): Unit ={
      val startNode = addNode(start)
      val endNode = addNode(end)
      val edge = new Edge(startNode, endNode, w)
      startNode.edges = edge :: startNode.edges
      edges = edge :: edges
    }

    def addEdge(start: Node[T], end: Node[T], w: Double) = {
      val edge = new Edge(start, end, w)
      if(!edges.contains(edge)){
        start.edges = edge :: start.edges
        edges = edge:: edges
      }

    }

    def pathCost(path: Path) = {
      path.cost
    }

    def pathsBetween(start: Node[T], end: Node[T]): Path = {
      val pq = PriorityQueue.empty[Path]( Ordering.by(pathCost))

      for(edge <- start.edges){
        var p = new Path(edge.startNode)
        p.addNode(edge.endNode, edge.weight)
        pq += p
      }

      var visited = mutable.Set.empty[Node[T]]

      while(pq.nonEmpty){
        var shortestPath: Path = pq.dequeue()
        val currentNode = shortestPath.path.last
        if(!visited.contains(currentNode)){
          visited += currentNode
          if(currentNode.value == end.value){
            return shortestPath
          }else{
            val neighbours = currentNode.edges.filterNot(edge => visited.contains(edge.endNode))
            for(n <- neighbours){
              shortestPath.addNode(n.endNode, n.weight)
              pq += shortestPath
            }
          }
        }

      }
      null
    }

    def printGraph(): Unit = {
      for(n <- nodes){
        for(e <- n.edges){
          print(e, " ")
        }
        println()
      }
    }

  }

}
