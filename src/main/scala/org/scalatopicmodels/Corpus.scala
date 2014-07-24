package org.scalatopicmodels

import java.io.{File, FileReader}
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Random
import edu.stanford.nlp.process.{CoreLabelTokenFactory, PTBTokenizer}
import breeze.linalg.DenseMatrix

/**
 * Created by alex on 24/05/14.
 */
class Corpus(docDirectory: String,minCountThreshold:Int) {

  var words: ListBuffer[Word] = ListBuffer.empty
  val numDocs=new File(docDirectory).listFiles.size
  var docTopicMatrix:DenseMatrix[Double]=_
  //var docTopicCounts: HashMap[(Int, Int), Int] = HashMap.empty
  //var wordTopicCounts: HashMap[(String, Int), Int] = HashMap.empty
  var topicWordMatrix:DenseMatrix[Double]=_
  //var docSize:HashMap[Int,Int]=HashMap.empty
  //var vocabulary: Set[String] = Set.empty
  var vocabulary:HashMap[String,Int]=Vocabulary.getVocabulary(docDirectory, minCountThreshold)




  /*
  def getVocabulary(minCountThreshold: Int) {
    vocabulary = Vocabulary.getVocabulary(docDirectory, minCountThreshold)
  }


  def incrementDocTopicCounts(doc: Int, topic: Int) {
    if (docTopicCounts.contains((doc, topic))) {
      docTopicCounts += ((doc, topic) -> (docTopicCounts((doc, topic)) + 1))
    }
    else {
      docTopicCounts += ((doc, topic) -> 1)
    }
  }


  def decrementDocTopicCounts(doc: Int, topic: Int) {
    if (docTopicCounts.contains((doc, topic))) {
      docTopicCounts += ((doc, topic) -> (docTopicCounts((doc, topic)) - 1))
    }
  }

  def incrementWordTopicCounts(word: String, topic: Int) {
    if (wordTopicCounts.contains((word, topic))) {
      wordTopicCounts += ((word, topic) -> (wordTopicCounts((word, topic)) + 1))
    }
    else {
      wordTopicCounts += ((word, topic) -> 1)
    }
  }

  def decrementWordTopicCounts(word: String, topic: Int) {
    if (wordTopicCounts.contains((word, topic))) {
      wordTopicCounts += ((word, topic) -> (wordTopicCounts((word, topic)) - 1))
    }
  }
*/
  def initialize(numTopics: Int) = {
    var docIndex = -1

    docTopicMatrix=DenseMatrix.zeros(numDocs,numTopics)
    topicWordMatrix=DenseMatrix.zeros(numTopics,vocabulary.size)

    println(docTopicMatrix.rows,docTopicMatrix.cols)


    def docProcessor(docFile: File) = {
      var dLength=0
      val randomTopicGenerator = new Random
      docIndex += 1

      val tokenizer = new PTBTokenizer(new FileReader(docFile), new CoreLabelTokenFactory(), "")

      while (tokenizer.hasNext) {
        dLength+=1
        val token = tokenizer.next.value().toLowerCase()

        val topic = randomTopicGenerator.nextInt(numTopics)

        if (vocabulary.contains(token)) {
          //Assign the word to a random topic
          words += Word(token, docIndex, topic)

          //update docTopic and wordTopic counters
          //incrementDocTopicCounts(docIndex, topic)
          //incrementWordTopicCounts(token, topic)
          docTopicMatrix(docIndex,topic)+=1.0
          topicWordMatrix(topic,vocabulary(token))+=1.0

        }

      }
      //docSize+=(docIndex -> dLength)
    }

    new File(docDirectory).listFiles.toIterator.filter(_.isFile).toList.map(docFile => docProcessor(docFile))




  }

}

