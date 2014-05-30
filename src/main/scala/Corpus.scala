import scala.collection.immutable.HashMap
import scala.util.Random
import scala.collection.mutable.ListBuffer

/**
 * Created by alex on 24/05/14.
 */
class Corpus(docs:List[String]){

  var wordTopicAssignments:ListBuffer[Word]=ListBuffer.empty
  var docTopicCounts:HashMap[(Int,Int),Int]=HashMap.empty
  var wordTopicCounts:HashMap[(String,Int),Int]=HashMap.empty

  def updateDocTopicCounts(doc:Int,topic:Int){
    if( docTopicCounts.contains((doc,topic)) ){
      docTopicCounts+=((doc,topic) -> (docTopicCounts((doc,topic))+1) )
    }
    else{
      docTopicCounts+=( (doc,topic) ->1)
    }
  }

  def updateWordTopicCounts(word:String,topic:Int){
    if( wordTopicCounts.contains((word,topic)) ){
      wordTopicCounts+=((word,topic) -> (wordTopicCounts((word,topic))+1) )
    }
    else{
      wordTopicCounts+=( (word,topic) ->1)
    }
  }

  def initialize(numTopics:Int)={
    var docIndex=0

    def docProcessor(doc:String)={
      val randomTopicGenerator= new Random
      docIndex+=1
      for(word <- doc.split(" ")){
        val topic=randomTopicGenerator.nextInt(numTopics)

        //Assign the word to a random topic
        wordTopicAssignments+=Word(word,topic,docIndex)
        //println(Word(word,topic,docIndex))

        //update docTopic and wordTopic counters
        updateDocTopicCounts(docIndex,topic)
        updateWordTopicCounts(word,topic)
      }
    }
    docs.map(doc => docProcessor(doc))
  }
}


