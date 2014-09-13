package com.topic.models.corpus

import com.topic.models.vocabulary.CountVocab
import breeze.linalg.DenseMatrix
import java.io.File
import scala.util.Random
import com.topic.models.tokenizer.StanfordTokenizer
import scala.collection.mutable.ListBuffer
import com.topic.models.word.Word
import scala.collection.immutable.HashMap

class CollapsedLDACorpus(numTopics: Int, docsDirectory: String, minCount: Int) extends StanfordTokenizer with Corpus {

  var numDocs = new File(docsDirectory).listFiles.size
  var vocabulary = CountVocab(docsDirectory, minCount).getVocabulary
  var docTopicMatrix = DenseMatrix.zeros(numDocs, numTopics)
  var topicWordMatrix = DenseMatrix.zeros(numTopics, vocabulary.size)
  var words: ListBuffer[Word] = ListBuffer.empty

  def initialize = {

    var docIndex = -1

    def docProcessor(docFile: File) = {
      var dLength = 0
      val randomTopicGenerator = new Random
      val tokenizer = new StanfordTokenizer
      val tokens = tokenizer.tokenizeFile(docFile)

      docIndex += 1

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
