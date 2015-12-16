package Summarizer

import org.tartarus.snowball._
import org.tartarus.snowball.ext.swedishStemmer

/**
 * Created by Jacob on 2015-12-07.
 */
object Tokenizer {

  val stemmer = new swedishStemmer()

  def stemWord(word: String): String = {
    stemmer.setCurrent(word)
    stemmer.stem()
    stemmer.getCurrent()
  }

  def getSentences(text:String):List[String] = {
     text.split('.') toList
  }

  def getStemmedWords(text: String):List[String] = {
    text.split(" ") filter(w => w.length > 3) map stemWord toList
  }
  def getWords(text: String):List[String] = {
    text.split(" ") toList
  }

  def getStemmedSentencesAndWords(text: String):List[List[String]] = {
    getSentences(text) map getStemmedWords
  }

  def getSentencesAndWords(text: String):List[List[String]] = {
    getSentences(text) map getWords
  }

}
