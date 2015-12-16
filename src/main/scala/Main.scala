/**
 * Created by Jacob on 2015-12-07.
 */

import Summarizer.Parser._
import Summarizer.Tokenizer._
import Types.Types._

import scala.reflect.io.File

object Main extends App{
val testText = "Dottern Susanna gifte sig 1607 med John Hall, en berömd läkare. Shakespeare gjorde paret till sina huvudarvingar, över hustrun och den andra dottern. Dottern Judith gifte sig med Thomas Quiney 10 februari 1616. De gifte sig i Holy Trinity Church. Pastorsadjunkten Richard Watts som senare vigde även Quigleys syster Mary var troligen officierande präst[27] Bröllopet ägde rum under fastan vilket var förbjudet. År 1616 hade fastan inletts den 23 januari och den avslutades den 7 april efter påsk. Detta innebar att äktenskapet krävde en särskild licens som kunde utfärdas av biskopen av Worcester, detta hade dock paret inte låtit göra. Walter Wright från Stratford dömdes för att ha vigts utan varken lysning eller licens så eftersom Quiney endast dömdes för att ha vigts utan licens har slutsatsen dragits att Quiney lät lysa i kyrkan[27] Detta regelbrott var av mindre betydelsefull art som förefaller ha orsakats av prästen då tre andra par också vigdes under februari. Quiney kallades dock ändå att infinna sig inför rätta i Worcester. Quiney underlät att infinna sig i rätten, och dömdes till exkommunikation. Det är inte känt huruvida även Judith blev bannlyst. Straffet för Quiney blev inte långvarigt, i november samma år visar källorna att paret var upptagna i kyrkan igen, och tilläts döpa sitt första barn.\n\nÄktenskapet började inte bra, Quiney hade kort dessförinnan gjort en annan kvinna, Margaret Wheeler, med barn. Wheeler dog när hon födde barnet som även det avled, och båda begrovs den 15 mars 1616. Den 26 mars 1616 ställdes Quiney inför rätta och han erkände då att han haft sex utom äktenskapet med Wheeler, och han förklarade sig villig att ta sitt straff. Han dömdes till ett skamstraff som skulle avtjänas inför församlingen tre söndagar i rad, och han tvingades också erkänna sin skuld inför prästen i hans hemförsamling[30] Merparten av straffet efterskänktes dock och hans slutliga straff kom att bestå av böter på fem shilling som gick till de fattiga.\n\nDetta bidrog till utformningen av det testamente, som undertecknades 25 mars samma år."
//
//  val summary = createGraph(tokenizedStemmedText)
  //    for(i <- summary){
  //        println(tokenizedText(i))
  //    }

  val sum = new Summarizer()

  println(sum.summarizeDocument("test.txt",  100))

  class Summarizer(){

    def summarizeDocument(path: String, targetWordCount: Int) = {
      val text = scala.io.Source.fromFile(path)("UTF-8").getLines.reduceLeft(_+_)
      //println(text)
      summarizeText(text, targetWordCount)
    }


    def summarizeText(text: String, targetWordCount: Int) = {
      val summaries = constructSummaries(text)
      val bestSummary = determineBestSummary(summaries, targetWordCount)
      bestSummary
    }

    def determineBestSummary(summaries: List[String], targetWordCount: Int) = {
      summaries.minBy(summary => {
        Math.abs(targetWordCount - summary.length)
      })

    }

    def constructSummaries(text: String) = {
      val tokenizedAndStemmedText = getStemmedSentencesAndWords(text).map(_.map(_.toLowerCase()))
      val tokenizedText = getSentences(text)
      println(tokenizedText mkString "\n")
      val graph: UniqueSentenceGraph = createGraph(tokenizedAndStemmedText)

      val firstSentenceNode = graph.getNodeFromIndex(1)
      val lastSentenceNode = graph.getNodeFromIndex(tokenizedAndStemmedText.length)

      val sentencePaths = graph.nPathsBetween(firstSentenceNode, lastSentenceNode, 8)
      //Construct list of all summaries
      val summaries = sentencePaths.map(x1 => x1.nodes.map(x => x.value._2-1))//First convert paths into lists of sentenceIndexes
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
