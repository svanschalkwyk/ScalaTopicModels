package org.scalatopicmodels.collapsedGibbsLDA

import java.io.{File, FileReader}
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Random
import edu.stanford.nlp.process.{CoreLabelTokenFactory, PTBTokenizer}
import breeze.linalg.{DenseMatrix, DenseVector}

/**
 * Created by alex on 24/05/14.
 */
class Corpus(docDirectory: String, minCountThreshold: Int) {

  var words: ListBuffer[Word] = ListBuffer.empty
  val numDocs = new File(docDirectory).listFiles.size
  var docTopicMatrix: DenseMatrix[Double] = _
  var topicWordMatrix: DenseMatrix[Double] = _
  var vocabulary: HashMap[String, Int] = Vocabulary.getVocabulary(docDirectory, minCountThreshold)

  def getDocTopic(doc:Int,topic:Int):Double=docTopicMatrix(doc,topic)

  def getTopicWord(topic:Int,word:Int):Double=topicWordMatrix(topic,word)

  def getDocTopicRow(doc:Int):DenseVector[Double]=docTopicMatrix(doc,::).t

  def getTopicWordRow(topic:Int):DenseVector[Double]=topicWordMatrix(topic,::).t

  def incrementDocTopic(doc: Int, topic: Int) {
    docTopicMatrix(doc, topic) += 1.0
  }

  def incrementTopicWord(topic: Int, word: Int) {
    topicWordMatrix(topic, word) += 1.0
  }

  def decrementDocTopic(doc: Int, topic: Int) {
    docTopicMatrix(doc, topic) -= 1.0
  }

  def decrementTopicWord(topic: Int, word: Int) {
    topicWordMatrix(topic, word) -= 1.0
  }

  def initialize(numTopics: Int) = {
    var docIndex = -1

    docTopicMatrix = DenseMatrix.zeros(numDocs, numTopics)
    topicWordMatrix = DenseMatrix.zeros(numTopics, vocabulary.size)

    println(docTopicMatrix.rows, docTopicMatrix.cols)


    def docProcessor(docFile: File) = {
      var dLength = 0
      val randomTopicGenerator = new Random
      docIndex += 1

      val tokenizer = new PTBTokenizer(new FileReader(docFile), new CoreLabelTokenFactory(), "")

      while (tokenizer.hasNext) {
        dLength += 1
        val token = tokenizer.next.value().toLowerCase()

        val topic = randomTopicGenerator.nextInt(numTopics)

        if (vocabulary.contains(token)) {

          //Assign the word to a random topic
          words += Word(token, docIndex, topic)
          docTopicMatrix(docIndex, topic) += 1.0
          topicWordMatrix(topic, vocabulary(token)) += 1.0

        }

      }
    }

    new File(docDirectory).listFiles.toIterator.filter(_.isFile).toList.map(docFile => docProcessor(docFile))

  }

  def reverseVocab: HashMap[Int, String] = {
    vocabulary.map(_ swap)
  }

}

