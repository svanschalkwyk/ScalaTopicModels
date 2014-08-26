package com.topic.models.Corpus

import com.topic.models.Vocabulary.CountVocab
import breeze.linalg.DenseMatrix
import java.io.File
import scala.util.Random
import com.topic.models.Tokenizer.StanfordTokenizer
import scala.collection.mutable.ListBuffer
import com.topic.models.Word.Word
import scala.collection.immutable.HashMap

class CollapsedLDACorpus(numTopics: Int, docsDirectory: String, minCount: Int) extends StanfordTokenizer with Corpus {

  protected val vocabulary = CountVocab(docsDirectory, minCount).getVocabulary
  protected var words: ListBuffer[Word] = ListBuffer.empty
  protected val numDocs = new File(docsDirectory).listFiles.size
  protected var docTopicMatrix: DenseMatrix[Double] = DenseMatrix.zeros(numDocs, numTopics)
  protected var topicWordMatrix: DenseMatrix[Double] = DenseMatrix.zeros(numTopics, vocabulary.size)

  def initialize = {
    var docIndex = -1

    def docProcessor(docFile: File) = {
      var dLength = 0
      val randomTopicGenerator = new Random
      docIndex += 1

      //val tokenizer = new PTBTokenizer(new FileReader(docFile), new CoreLabelTokenFactory(), "")
      val tokenizer = new StanfordTokenizer
      val tokens = tokenizer.tokenizeFile(docFile)

      for (token <- tokens) {
        dLength += 1
        val topic = randomTopicGenerator.nextInt(numTopics)

        if (vocabulary.contains(token)) {

          //Assign the word to a random topic
          words += Word(token, docIndex, topic)
          docTopicMatrix(docIndex, topic) += 1.0
          topicWordMatrix(topic, vocabulary(token)) += 1.0

        }

      }
    }

    new File(docsDirectory).listFiles.toIterator.filter(_.isFile).toList.map(docFile => docProcessor(docFile))
  }

  def reverseVocab: HashMap[Int, String] = {
    vocabulary.map(_ swap)
  }
}
