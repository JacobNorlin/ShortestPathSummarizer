/**
 * Created by Jacob on 2015-12-07.
 */

import Summarizer.Parser._
import Summarizer.Tokenizer._
import Types.Types._

import scala.reflect.io.File

object Main extends App{


  val sum = new Summarizer()

  println(sum.summarizeDocument("test6",  100))

  class Summarizer(){

    def summarizeAnnotatedDocument(path:String, targetWordCount: Int) = {

    }

    def summarizeDocument(path: String, targetWordCount: Int) = {
      val text = scala.io.Source.fromFile(path)("UTF-8").getLines.reduceLeft(_+_)
      //println(text)
      summarizeText(text, targetWordCount)
    }

    def countWords(text: String) = {
      text.split(' ').length
    }


    def summarizeText(text: String, targetWordCount: Int) = {
      val summaries = constructSummaries(text)
      val bestSummary = determineBestSummary(summaries, targetWordCount)
      bestSummary
    }

    def determineBestSummary(summaries: List[String], targetWordCount: Int) = {
      println(summaries.map(countWords) mkString "\n")
      summaries.minBy(summary => {
        Math.abs(targetWordCount - countWords(summary))
      })

    }

    def constructSummaries(text: String) = {
      var tokenizedAndStemmedText = getStemmedSentencesAndWords(text).map(_.map(_.toLowerCase()))
      val tokenizedText = getSentences(text).tail
      getHeaders(text)
      val title = tokenizedAndStemmedText.head
      val graph: UniqueSentenceGraph = createGraph(tokenizedAndStemmedText.tail, title)

      val firstSentenceNode = graph.getNodeFromIndex(1)

      println(firstSentenceNode.value._1)
      val lastSentenceNode = graph.getNodeFromIndex(tokenizedAndStemmedText.length -1)

      val sentencePaths = graph.nPathsBetween(firstSentenceNode, lastSentenceNode, 16)
      println(sentencePaths.map(x1 => x1.nodes.map(x => x.value._2-1)) mkString "\n")
      //Construct list of all summaries
      val summaries = sentencePaths.map(x1 => x1.nodes.map(x => x.value._2-1).sorted)//First convert paths into lists of sentenceIndexes
        .map(path => {
        convertPathToSummary(path, tokenizedText)
        })


      summaries
    }

    def convertPathToSummary(path: List[Int], tokenizedText: List[String]) = {
      path.map(sentenceIndex => tokenizedText(sentenceIndex))
        .fold("")((s1, s2) => {
          s1 + ". " + s2 //Concatenate sentences back together with a . between them.
        })
    }



  }


}
