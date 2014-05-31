import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * Created by alex on 24/05/14.
 */
class Corpus(docs:List[String]){

  var words:ListBuffer[Word]=ListBuffer.empty
  var docTopicCounts:HashMap[(Int,Int),Int]=HashMap.empty
  var wordTopicCounts:HashMap[(String,Int),Int]=HashMap.empty


  def incrementDocTopicCounts(doc:Int,topic:Int){
    if( docTopicCounts.contains((doc,topic)) ){
      docTopicCounts+=((doc,topic) -> (docTopicCounts((doc,topic))+1) )
    }
    else{
      docTopicCounts+=( (doc,topic) ->1)
    }
  }


  def decrementDocTopicCounts(doc:Int,topic:Int){
    if( docTopicCounts.contains((doc,topic)) ){
      docTopicCounts+=((doc,topic) -> (docTopicCounts((doc,topic))-1) )
    }
  }

  def incrementWordTopicCounts(word:String,topic:Int){
    if( wordTopicCounts.contains((word,topic)) ){
      wordTopicCounts+=((word,topic) -> (wordTopicCounts((word,topic))+1) )
    }
    else{
      wordTopicCounts+=( (word,topic) ->1)
    }
  }

  def decrementWordTopicCounts(word:String,topic:Int){
    if( wordTopicCounts.contains((word,topic)) ){
      wordTopicCounts+=((word,topic) -> (wordTopicCounts((word,topic))-1) )
    }
  }

  def initialize(numTopics:Int)={
    var docIndex=0

    def docProcessor(doc:String)={
      val randomTopicGenerator= new Random
      docIndex+=1
      //remove all punctuation at end of word
      for(word <- doc.replaceAll("(\\w+)\\p{Punct}(\\s|$)", "$1$2").split(" ")){

        val topic=randomTopicGenerator.nextInt(numTopics)

        //Assign the word to a random topic
        words+=Word(word,docIndex,topic)

        //update docTopic and wordTopic counters
        incrementDocTopicCounts(docIndex,topic)
        incrementWordTopicCounts(word,topic)
      }
    }
    docs.map(doc => docProcessor(doc))
  }


}

object test extends App{
  val testCorpus=List("hello? hello my name is Alex","I am hello testing my corpus class","Let's see if hello it works")

  val myCorpus=new Corpus(testCorpus)

  myCorpus.initialize(3)

  println(myCorpus.words)
  println(myCorpus.docTopicCounts)
  println(myCorpus.wordTopicCounts)

}

