package Summarizer

/**
 * Created by Jacob on 2015-12-07.
 */
object SentenceLikeness {

  val title = List("")


  def getSameWords(s1: List[String], s2: List[String]) : List[String] = {
    s1 intersect s2
  }

  def similarWords(s1: List[String], s2: List[String]) : Int = {
    getSameWords(s1, s2).length
  }


  def termFrequency(word: String, text: List[List[String]]) : Int = {
    text.map(s => {
      termFrequencyInSentence(word, s)
    }) sum
  }

  def termFrequencyInSentence(word: String, sentence: List[String]) : Int = {
    sentence count (_ == word)
  }


  def early(j: Int) = {
    if(j < 10)
      2
    else
      1
  }


  def sentenceWeight(indexedSentence:(List[String], Int), text:List[List[String]], occurenceMatrix: Array[Array[Int]], wordCount: Map[String, Int]): Double = {

    val totalWordCount = text.flatten.flatten.length
    val s = indexedSentence._1
    (1 + getSameWords(s, title).length) *
    (1 + s.map(w => wordCount(w)).sum / totalWordCount) *
    early(text.indexOf(s)) * Math.sqrt(1 + occurenceMatrix(indexedSentence._2 - 1).count(_ > 0) - 1)
  }

  def cost(s1: (List[String], Int), s2: (List[String], Int), text:List[List[String]], occurenceMatrix: Array[Array[Int]], wordCount: Map[String, Int]) = {
    val i = s1._2 - 1
    val j = s2._2 - 1
    Math.pow((i - j),2) / (occurenceMatrix(i)(j) * sentenceWeight(s2, text, occurenceMatrix, wordCount))
  }

}
