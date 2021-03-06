package Summarizer

import Types.Types._
import Summarizer.SentenceLikeness._
/**
 * Created by Jacob on 2015-12-07.
 */

object Parser {

  /**
   * Creates a map(dictionary) of how many times each word occurs
   * @param text
   * @return
   */
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
    //maps each sentence pair to its similarity value
    sentences.map(s1 => {
      sentences.map(s2 => {
        if(s1._2 != s2._2)
          similarityMeasure(s1._1, s2._1)
        else
          0 //We dont care how many words a sentence shares with itself
      }) toArray
    }) toArray
  }

  /**
   * Constructs a node for each sentence
   * @param sentences
   * @return
   */
  def buildInitialGraph(sentences: List[IndexedSentence]) = {
    val g = new UniqueSentenceGraph()
    for(s <- sentences){
      g.addNode(s)
    }
    g
  }


  def createGraph(text: List[Sentence], title: Sentence) = {
    val indexedSentences = indexSentences(text)
    val allWords: Set[String] = (for (s <- text;
                                      w <- s) yield w).toSet
    val wordCount: Map[String, Int] = countOfEachWord(text)
    val totalWordCount = text.flatMap(_.map(x => termFrequency(x, text))) sum
    val similarityMatrix = buildSimilarityMatrix(indexedSentences, similarWords)
    val g = buildInitialGraph(indexedSentences)
    //This is to make sure there is at least one path from the first to last sentence

    for ((s, i) <- indexedSentences) {
      if (i < indexedSentences.length) {
        val next = indexedSentences(i) //this is because sentences are 1 indexed
        g.addEdge(g.getNodeFromIndex(i), g.getNodeFromIndex(i + 1), cost((s, i), next, text, similarityMatrix, wordCount, title, totalWordCount))
      }
    }
    println("inital graph done")

    for (n1 <- g.nodes) {
      for (n2 <- g.nodes) {
        val t1 = n1.value
        val t2 = n2.value
        //If sentences are similar they will share an edge
        if (similarityMatrix(t1._2 - 1)(t2._2 - 1) >= 1 && t1._2 != t2._2) {
          val c = cost(t1, t2, text, similarityMatrix, wordCount, title, totalWordCount)
          g.addEdge(n1, n2, c)
        }
      }
    }

    println("Graph constructed")
    g
  }



}
