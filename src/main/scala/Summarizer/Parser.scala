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

  def indexSentences(text: List[Sentence]) = {
    text.zip(Range(1, text.length+1))
  }

  def buildSimilarityMatrix(sentences: List[IndexedSentence], similarityMeasure: (Sentence, Sentence) => Double) = {
    sentences.map(s1 => {
      sentences.map(s2 => {
        if(s1._2 != s2._2)
          similarityMeasure(s1._1, s2._1)
        else
          0 //We dont how many words a sentence shares with itself
      })
    })
  }



  def createGraph(text: List[Sentence]) = {
    val indexedSentences = indexSentences(text)
    val allWords: Set[String] = (for(s <- text;
                                 w <- s) yield w).toSet
    val wordCount: Map[String, Int] = countOfEachWord(text)

    val similarityMatrix = buildSimilarityMatrix(indexedSentences, similarWords)

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
