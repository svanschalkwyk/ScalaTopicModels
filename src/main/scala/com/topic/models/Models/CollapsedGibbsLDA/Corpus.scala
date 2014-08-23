package com.topic.models.Models.CollapsedGibbsLDA

import java.io.{File, FileReader}
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Random
import edu.stanford.nlp.process.{CoreLabelTokenFactory, PTBTokenizer}
import breeze.linalg.{Axis, sum, DenseMatrix, DenseVector}
import com.topic.models.Tokenizer.StanfordTokenizer


/**
 * Corpus class for collapsed Gibbs Sampling LDA.  Creates and initializes word/topic assignments and vocabulary.
 */
class Corpus(docDirectory: String, minCountThreshold: Int) extends StanfordTokenizer{

  private var words: ListBuffer[Word] = ListBuffer.empty
  private val numDocs = new File(docDirectory).listFiles.size
  private var docTopicMatrix: DenseMatrix[Double] = _
  private var topicWordMatrix: DenseMatrix[Double] = _
  var vocabulary: HashMap[String, Int] = Vocabulary.getVocabulary(docDirectory, minCountThreshold)

  def getWords:ListBuffer[Word]=words

  def getNumDocs:Int=numDocs

  def getDocTopicMatrix:DenseMatrix[Double]=docTopicMatrix

  def getTopicWordMatrix:DenseMatrix[Double]=topicWordMatrix

  def getDocTopic(doc:Int,topic:Int):Double=docTopicMatrix(doc,topic)

  def getTopicWord(topic:Int,word:Int):Double=topicWordMatrix(topic,word)

  def getDocTopicRow(doc:Int):DenseVector[Double]=docTopicMatrix(doc,::).t

  def getTopicWordRow(topic:Int):DenseVector[Double]=topicWordMatrix(topic,::).t

  def getTopicWordCol(word:Int):DenseVector[Double]=topicWordMatrix(::,word)

  def getTopicSums=sum(topicWordMatrix, Axis._1)

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

  def setDocTopicRow(rowIdx:Int,newRow:DenseVector[Double]){
    docTopicMatrix(rowIdx,::):=newRow.t
  }

  def setTopicWordRow(rowIdx:Int,newRow:DenseVector[Double]){
    topicWordMatrix(rowIdx,::):=newRow.t
  }

  /**
   * Randomly initialize word/topic assignments.
   * @param numTopics Number of possible topics words can be assigned to.
   */
  def initialize(numTopics: Int) = {
    var docIndex = -1

    docTopicMatrix = DenseMatrix.zeros(numDocs, numTopics)
    topicWordMatrix = DenseMatrix.zeros(numTopics, vocabulary.size)

    println(docTopicMatrix.rows, docTopicMatrix.cols)


    def docProcessor(docFile: File) = {
      var dLength = 0
      val randomTopicGenerator = new Random
      docIndex += 1

      //val tokenizer = new PTBTokenizer(new FileReader(docFile), new CoreLabelTokenFactory(), "")
      val tokenizer=new StanfordTokenizer
      val tokens=tokenizer.tokenizeFile(docFile)

      while (tokens.hasNext) {
        dLength += 1
        val token = tokens.next.value().toLowerCase()

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

  /**
   * Reverse vocabulary so that integer ids are mapped to word strings.
   */
  def reverseVocab: HashMap[Int, String] = {
    vocabulary.map(_ swap)
  }

}

