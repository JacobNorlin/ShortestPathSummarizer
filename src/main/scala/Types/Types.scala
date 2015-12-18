package Types

import scala.collection.mutable.Set
import scala.collection.mutable.PriorityQueue
/**
 * Created by Jacob on 2015-12-07.
 */
object Types {

  type Text = List[Sentence]
  type Sentence = List[Word]
  type IndexedSentence = (Sentence, Int)
  type Word = String

  /**
   * Super specific graph implementation for this problem
   */
  class UniqueSentenceGraph{

    /**
     * Class for nodes. Is not a case class since the edge set has to be updated, which it cant if its a val. Maybe im just bad a scala.
     * Edges are a set, because you should not be able to have duplicate edges
     * @param v value of node, an indexed sentence
     * @param es Set of edges
     */
    class Node(v: IndexedSentence, es: Set[Edge]){
      var edges = es
      val value = v
      override def equals(o: Any) = o match{
        case that: Node => that.value._2 == this.value._2 //nodes are considered equal if they contain the same sentence
        case _ => false
      }
      override def hashCode = value._2 //hashed to the sentence index, since sentences are unique in that regard.
      override def toString: String = {
        value toString
      }
    }

    /**
     * Class to represent edge
     * @param start Starting node
     * @param end End node
     * @param cost Cost of path
     */
    case class Edge(start: Node, end: Node, cost: Double){
      override def equals(o: Any) = o match{
        case that: Edge => that.start.equals(start) && that.end.equals(end) //Uses node similarity to determine edge similarity
        case _ => false
      }
      override def toString: String = {
        "" + start.value._2 + "->("+cost+")-> " + end.value._2
      }
    }

    /**
     * Small class for keeping track of paths
     * @param nodes Nodes in the path, in order of the path.
     * @param current Current path node
     * @param cost Total path cost
     */
    case class Path(nodes: List[Node], current: Node, cost: Double){
      override def equals(o: Any) = o match{
        case that: Path => that.nodes.map(n => n.value._2).sorted.equals(this.nodes.map(n=>n.value._2)) //A path is equal if it contains the same nodes as another path
        case _ => false
      }
    }

    /**
     * Sets for nodes and edges. Since sentences are unique, there should never be any node or edge duplicates.
     */
    var nodes : Set[Node] = Set.empty
    var edges : Set[Edge] = Set.empty

    /**
     * Method for getting sentence i out of graph
     * @param index sentence index
     * @return returns i:th sentence from graph
     */
    def getNodeFromIndex(index: Int) = {
      nodes.filter(n => n.value._2 == index).head
    }

    def addEdge(start: Node, end: Node, cost: Double): Unit ={
      val edge = Edge(start, end, cost)
      start.edges += edge
      //end.edges += edge

      edges += edge

    }
    def addNode(value: IndexedSentence) = {
      nodes += new Node(value, Set.empty)
    }

    def printGraph(): Unit = {
      for(n <- nodes){
        for(e <- n.edges){
          print(e, " ")
        }
        println()
      }
    }

    //Used for the priority queue to order it by path cost.
    def pathOrder(p: Path) = {
      p.cost
    }

    def nPathsBetween(start: Node, end: Node, n: Int): List[Path] = {
      val pq = PriorityQueue.empty[Path]( Ordering.by(pathOrder))
      var shortestPaths = List.empty[Path]
      for(edge <- start.edges){
        pq += Path(List(edge.start), edge.end, edge.cost)
      }
      //Set used to keep track of paths with loops so we can avoid them.
      var pathsWithLoops = Set.empty[Path]
      var i = 0
      while(pq.nonEmpty){
        val shortestPath = pq.dequeue()
        val currentNode = shortestPath.current

        val currentPath = Path(shortestPath.nodes ++ List(currentNode), currentNode, shortestPath.cost)
        //Avoid all paths with loops
        if(shortestPath.nodes.contains(currentNode)){
          pathsWithLoops += currentPath

        }
        if(!pathsWithLoops.contains(currentPath)){
          if(currentNode.equals(end)){
            println("found path")
            i += 1
            shortestPaths = currentPath :: shortestPaths
            if(n == i){//If we have met the goal we terminate.
              return shortestPaths
            }

          }
          val neighbours = currentNode.edges
          for(n <- neighbours){
            pq += Path(shortestPath.nodes ++ List(currentNode), n.end, shortestPath.cost + n.cost)
          }
        }
      }
      shortestPaths
    }

  }



}
