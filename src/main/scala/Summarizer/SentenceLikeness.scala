package Summarizer

import Types.Types._
/**
 * Created by Jacob on 2015-12-07.
 */
object SentenceLikeness {

  val title = List("")


  def getSameWords(s1: Sentence, s2: Sentence) : Sentence = {
    s1 intersect s2
  }

  def similarWords(s1: Sentence, s2: Sentence) : Int = {
    getSameWords(s1, s2).length
  }


  def termFrequency(word: String, text: List[Sentence]) : Int = {
    text.map(s => {
      termFrequencyInSentence(word, s)
    }) sum
  }

  def termFrequencyInSentence(word: String, sentence: Sentence) : Int = {
    sentence count (_ == word)
  }


  def early(j: Int) = {
    if(j < 10)
      2
    else
      1
  }

  /**
   * Calculates the weight of a sentence based on classic summarization methods
   * @param sentence
   * @param text
   * @param occurenceMatrix
   * @param wordCount
   * @return
   */
  def sentenceWeight(sentence:IndexedSentence, text:List[Sentence], occurenceMatrix: Array[Array[Int]], wordCount: Map[Word, Int]): Double = {

    val totalWordCount = text.flatten.flatten.length
    val s = sentence._1
    (1 + getSameWords(s, title).length) *
    (1 + s.map(w => wordCount(w)).sum / totalWordCount) *
    early(text.indexOf(s)) * Math.sqrt(1 + occurenceMatrix(sentence._2 - 1).count(_ > 0) - 1)
  }

  def cost(s1: IndexedSentence, s2: IndexedSentence, text:List[Sentence], occurenceMatrix: Array[Array[Int]], wordCount: Map[Word, Int]) = {
    val i = s1._2 - 1
    val j = s2._2 - 1
    Math.pow((i - j),2) / (occurenceMatrix(i)(j) * sentenceWeight(s2, text, occurenceMatrix, wordCount))
  }

}
