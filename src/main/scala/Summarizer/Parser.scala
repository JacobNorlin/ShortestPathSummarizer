package Summarizer

import Types.Types._
import Summarizer.SentenceLikeness._
/**
 * Created by Jacob on 2015-12-07.
 */

object Parser {

  def countOfEachWord(text: List[List[String]]) = {
    (for(s <- text;
         w <- s)
      yield w -> termFrequency(w, text)
      ) toMap
  }

  def createGraph(text: List[List[String]]) = {
    val indexedSentences : List[(List[String], Int)]  = text.zip(Range(1, text.length+1))

    val allWords: Set[String] = (for(s <- text;
                                 w <- s) yield w).toSet
    val wordCount: Map[String, Int] = countOfEachWord(text)

    //Sum rows or whatever to get amount of edges for node
    val similarityMatrix: Array[Array[Int]] = Array.ofDim(indexedSentences.length, indexedSentences.length)

    for((s1,i) <- indexedSentences){
      for((s2,j) <- indexedSentences){
        val a = similarWords(s1, s2)
        similarityMatrix(i-1)(j-1) = a
        print(a)
      }
      println()
    }

    val graph = new Graph[(List[String], Int)]()
    //This is to make sure there is at least one path from the first to last sentence
    for(s <- indexedSentences){
      if(s._2 < indexedSentences.length){
        val next = indexedSentences(s._2)
        graph.addEdge(s, next, cost(s, next, text, similarityMatrix, wordCount))
      }

    }

    for(n1 <- graph.nodes){
      for(n2 <- graph.nodes){
        val t1 = n1.value
        val t2 = n2.value
        //If sentences are similar they will share an edge
        if(similarityMatrix(t1._2 -1 )(t2._2 - 1) >= 1 && t1._2 != t2._2){
          val c = cost(t1, t2, text, similarityMatrix, wordCount)
          graph.addEdge(n1, n2, c)
        }
      }

    }

    graph.printGraph()
    graph.pathsBetween(graph.nodes.last, graph.nodes.head).path.map(x => x.value._2-1)

  }



}
