package Summarizer

import Types.Types._
/**
 * Created by Jacob on 2015-12-07.
 */
object SentenceLikeness {



  def sameWords(s1: Sentence, s2: Sentence) : Sentence = {
    s1 intersect s2
  }

  def similarWords(s1: Sentence, s2: Sentence) : Double = {
    sameWords(s1, s2).length toDouble
  }

  def cosineSimilarity(s1: Sentence, s2: Sentence, text: List[Sentence]): Double = {
    def v1 = s1.map(w => termFrequency(w, text))
    def v2 = s2.map(w => termFrequency(w, text))
    val t = v1.zip(v2).map(x => x._1*x._2) sum
    val n = Math.sqrt(v1.map(Math.pow(_,2)).sum)*math.sqrt(v2.map(Math.pow(_,2)).sum)
    t / n
  }


  def termFrequency(word: String, text: List[Sentence]) : Int = {
    if(word.length >= 4){
      text.map(s => {
        termFrequencyInSentence(word, s)
      }) sum
    }else
      0

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
  def sentenceWeight(sentence:IndexedSentence, text:List[Sentence], similarityMatrix: Array[Array[Double]], wordCount: Map[Word, Int], title: Sentence, totalWordCount: Int): Double = {

    //val totalWordCount = wordCount.filter(x => x._1.length > 3)
    val s = sentence._1
    //Complicated formula
    (1 + sameWords(s, title).length) *
    (1 + s.map(w => wordCount(w)).sum / totalWordCount) *
    early(text.indexOf(s)) * Math.sqrt(1 + similarityMatrix(sentence._2 - 1).count(_ > 0) - 1)
  }

  def cost(s1: IndexedSentence, s2: IndexedSentence, text:List[Sentence], similarityMatrix: Array[Array[Double]], wordCount: Map[Word, Int], title: Sentence, totalWordCount: Int) = {
    val i = s1._2 - 1
    val j = s2._2 - 1
    Math.pow((i - j),2) / (similarityMatrix(i)(j) * sentenceWeight(s2, text, similarityMatrix, wordCount, title, totalWordCount))
  }

}
