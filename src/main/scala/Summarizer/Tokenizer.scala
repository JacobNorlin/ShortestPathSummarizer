package Summarizer

import org.tartarus.snowball.ext.swedishStemmer

import scala.util.matching.Regex

/**
 * Created by Jacob on 2015-12-07.
 */
object Tokenizer {

  val stemmer = new swedishStemmer()

  def stemWord(word: String): String = {
    //This is gr8
    stemmer.setCurrent(word)
    stemmer.stem()
    stemmer.getCurrent()
  }

  //Does not work, is not used. Should return all sentences annotated by <h></h>
  def getHeaders(text: String) = {
    val regex = new Regex("""<h>[a-zA-Z0-9\W]*<\/h>""")

    for(m <- regex.findAllIn(text)){
      println(m)
    }
  }

  def getSentences(text:String):List[String] = {
    //This is a very naive sentence boundary. It just checks if there is a capitalized letter after a . ? or !
    text.split("""[\.\!\?] (?=[A-Z*])""") toList
  }

  def getStemmedWords(text: String):List[String] = {
    text.split(" ") filter(w => w.length > 1) map stemWord toList
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
