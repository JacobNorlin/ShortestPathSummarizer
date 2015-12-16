/**
 * Created by Jacob on 2015-12-07.
 */

import Summarizer.Parser._
import Summarizer.Tokenizer._

import scalax.collection.Graph
object Main extends App{
    val testText = "Shakespeare föddes i ett hus på Henley Street i Stratford-upon-Avon i Warwickshire. Modern Mary Arden tillhörde en besutten katolsk släkt[2], troligen samma släkt Arden som adlats av Vilhelm Erövraren. Fadern John Shakespeare var handskmakare och fastighetsägare, och innehade tidvis ämbeten i staden.[3] William hade två äldre systrar som avled i unga år. Williams dop förrättades 26 april 1564 i Heliga Trefaldighetens kyrka i Stratford. Han fick senare fem syskon till, tre bröder och två systrar.[4]\n\nShakespeare utbildades sannolikt vid Stratfords grammar school som grundats av stadens gillen på 1200-talet, och han fick troligen sina kunskaper i latin där.[5] Det finns emellertid inga matriklar som kan styrka detta, utan det är ett antagande som bygger på förhållanden i staden och faderns sociala ställning. Fadern, som kom från en arrenderande bondeklass, hade skaffat sig lite pengar, gifte sig rikt med dottern till godsägaren han arrenderat av och fick med tiden betydande förtroendeuppdrag i staden. Men framgången i faderns affärer var ombytlig, och enligt traditionen fick den unge Shakespeare avbryta studierna vid elva års ålder. Under något år ska Shakespeare enligt traditionen ha försörjt sig som lärare.[6]\n\nFadern blev 1592 indragen i en rättsprocess, anklagad för bristande kyrksamhet. Även William Shakespeare ska enligt legenden ha varit i klammeri med rättvisan, då han dömdes till spöstraff efter en olovlig jakt i Sir Thomas Lucys hjortpark.[7]\n\nMånga uppgifter om William Shakespeares liv är osäkra, men genom arkiv har en del säkra fakta om honom kunnat erhållas. Vissa forskare ställer sig dock kritiska till att det skulle vara han som är författaren bakom dramerna (se vidare avsnittet Författarfrågan). Utöver dessa oklarheter tillkommer alla de rykten och legender som traditionen vävt in i hans levnadsberättelse, och som ofta utgör merparten av hans biografi.".toLowerCase()
    val tokenizedStemmedText = getStemmedSentencesAndWords(testText)
    val tokenizedText = getSentences(testText)

    val summary = createGraph(tokenizedStemmedText).distinct.sorted
    for(i <- summary){
        println(tokenizedText(i))
    }



}
