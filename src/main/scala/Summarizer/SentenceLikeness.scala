package Summarizer

import Types.Types._
/**
 * Created by Jacob on 2015-12-07.
 */
object SentenceLikeness {

  val title = List("")


  def sameWords(s1: Sentence, s2: Sentence) : Sentence = {
    s1 intersect s2
  }

  def similarWords(s1: Sentence, s2: Sentence) : Double = {
    sameWords(s1, s2).length toDouble
  }


  def termFrequency(word: String, text: List[Sentence]) : Int = {
    text.map(s => {
      termFrequencyInSentence(word, s)
    }) sum
  }

  def termFrequencyInSentence(word: String, sentence: Sentence) : Int = {
    sentence count (_ == word)
  }

  /**
   * Simple function that gives earlier sentences a higher value
   * @param j
   * @return
   */
  def early(j: Int) = {
    if(j < 10)
      2
    else
      1
  }

  /**
   *
   * @param sentence
   * @param text
   * @param similarityMatrix
   * @param wordCount
   * @return
   */
  def sentenceWeight(sentence:IndexedSentence, text:List[Sentence], similarityMatrix: List[List[Double]], wordCount: Map[Word, Int]): Double = {

    val totalWordCount = text.flatten.flatten.length
    val s = sentence._1
    //Complicated formula
    (1 + sameWords(s, title).length) *
    (1 + s.map(w => wordCount(w)).sum / totalWordCount) *
    early(text.indexOf(s)) * Math.sqrt(1 + similarityMatrix(sentence._2 - 1).count(_ > 0) - 1)
  }

  def cost(s1: IndexedSentence, s2: IndexedSentence, text:List[Sentence], similarityMatrix: List[List[Double]], wordCount: Map[Word, Int]) = {
    val i = s1._2 - 1
    val j = s2._2 - 1
    Math.pow((i - j),2) / (similarityMatrix(i)(j) * sentenceWeight(s2, text, similarityMatrix, wordCount))
  }

}
