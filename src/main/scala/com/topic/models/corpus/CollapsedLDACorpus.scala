package com.topic.models.corpus

import scala.util.Random
import com.topic.models.word.Word
import com.topic.models.utils.DocUtils
import scala.collection.immutable.HashMap
import scala.io.Source._
import com.topic.models.tokenizer.StanfordTokenizer
import java.io.File
import breeze.linalg.DenseMatrix

class CollapsedLDACorpus(vocab: HashMap[String, Int], numTopics: Int, docDirectory: String) extends Corpus {

  val numDocs = DocUtils.numDocs(docDirectory)
  val vocabulary = vocab
  var docTopicMatrix = DenseMatrix.zeros[Double](numDocs, numTopics)
  var topicWordMatrix = DenseMatrix.zeros[Double](numTopics, vocabulary.size)
  var words: Vector[Word] = Vector.empty

  val randomTopicGenerator = new Random
  var docIndex = 0


  def processDoc(contents: String) = {

    val tokens = StanfordTokenizer.tokenizeString(contents)

    for (token <- tokens) {

      val randTopic = randomTopicGenerator.nextInt(numTopics)

      if (vocabulary.contains(token)) {

        //Assign the word to a random topic
        words :+= Word(token, docIndex, randTopic)
        docTopicMatrix(docIndex, randTopic) += 1.0
        topicWordMatrix(randTopic, vocabulary(token)) += 1.0
      }
    }
    docIndex += 1
  }

  def initialize = {
    new File(docDirectory).listFiles.toIterator.filter(_.getName.endsWith(".txt")).toList.map(docFile => processDoc(fromFile(docFile).getLines().mkString))
  }

  def reverseVocab: HashMap[Int, String] = {
    vocabulary.map(_ swap)
  }
}


