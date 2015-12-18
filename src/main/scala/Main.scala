/**
 * Created by Jacob on 2015-12-07.
 */

import Summarizer.Parser._
import Summarizer.Tokenizer._
import Types.Types._

import scala.reflect.io.File

object Main extends App{


  val sum = new Summarizer()

  override def main(args: Array[String]) = {
    var path: String = ""
    var goal: Int = 0
    var paths: Int = 4
    args.length match {
      case 3 => {
        path = args(0)
        goal = args(1).toInt
        paths = args(2).toInt
      }
      case 2 => {
        path = args(0)
        goal = args(1).toInt
      }
      case _ => {
        println("Please give either (path, goal) or (path, goal, numPaths) number of arguments")

      }
    }
    if(!path.equals("")){
      val sum = new Summarizer()
      val summary = sum.summarizeDocument(path, goal, paths)
      println(summary)
      println("Number of words in summary: "+sum.countWords(summary))
    }

  }



  class Summarizer(){


    def summarizeDocument(path: String, targetWordCount: Int, numPaths: Int) = {
      val text = scala.io.Source.fromFile(path)("UTF-8").getLines.reduceLeft(_+_)
      summarizeText(text, targetWordCount, numPaths)
    }

    def countWords(text: String) = {
      text.split(' ').length
    }


    def summarizeText(text: String, targetWordCount: Int, numPaths: Int) = {
      val summaries = constructSummaries(text, numPaths)
      val bestSummary = determineBestSummary(summaries, targetWordCount)
      bestSummary
    }

    def determineBestSummary(summaries: List[String], targetWordCount: Int) = {
      println("Paths of length:")
      println(summaries.map(countWords) mkString "\n")
      summaries.minBy(summary => { //sorts summaries according to distance to target word count
        Math.abs(targetWordCount - countWords(summary))
      })

    }

    /**
     * Constructs a number of summaries based on shortest path summarization
     * @param text
     * @return
     */
    def constructSummaries(text: String, numPaths: Int) = {
      var tokenizedAndStemmedText = getStemmedSentencesAndWords(text).map(_.map(_.toLowerCase()))
      val tokenizedText = getSentences(text).tail
      val title = tokenizedAndStemmedText.head
      val graph: UniqueSentenceGraph = createGraph(tokenizedAndStemmedText.tail, title)

      val firstSentenceNode = graph.getNodeFromIndex(1)

      val lastSentenceNode = graph.getNodeFromIndex(tokenizedAndStemmedText.length -1)

      val sentencePaths = graph.nPathsBetween(firstSentenceNode, lastSentenceNode, numPaths)
      //println(sentencePaths.map(x1 => x1.nodes.map(x => x.value._2-1)) mkString "\n")
      //Construct list of all summaries
      val summaries = sentencePaths.map(x1 => x1.nodes.map(x => x.value._2-1))//First convert paths into lists of sentenceIndexes
        .map(path => {
        convertPathToSummary(path, tokenizedText)
        })


      summaries
    }

    /**
     * Takes a list of sentence indexes and returns a string of those sentences
     * @param path
     * @param tokenizedText
     * @return
     */
    def convertPathToSummary(path: List[Int], tokenizedText: List[String]) = {
      path.map(sentenceIndex => tokenizedText(sentenceIndex))
        .fold("")((s1, s2) => {
          s1 + ". " + s2 //Concatenate sentences back together with a . between them.
        }).substring(2)//remove inital dot
    }



  }


}
